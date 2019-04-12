-- Implementation of the "Server Command Reference" spec
-- The spec changes very, very infrequently, but the reference is
-- included in the source of this file
-- 
-- Typically you want to import this qualified, e.g. as 'SCCmd',
-- for 'SCCmd.quit', etc.

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Vivid.SC.Server.Commands (
     quit
   , dumpOSC
   , DumpOSCMode(..)
   , sync
   , clearSched
   , d_recv
   , d_load
   , d_loadDir
   , n_free
   , n_run
   , n_set

   , addActionInt
   , AddAction(..)
   , s_new
   , g_new
   , p_new
   , g_freeAll

   -- * Buffer commands
   , b_alloc
   , b_allocRead
   , b_zero
   , b_free
   , b_close
   , b_query
   , b_write
   ) where

import Vivid.OSC

import Vivid.SC.Server.Types -- (SyncId(..))
import Vivid.SC.SynthDef.Literally (LiteralSynthDef, SynthDefFile(..), encodeSynthDefFile)

import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import Data.Int (Int32)
import Data.Maybe

{-
title:: Server Command Reference
summary:: SuperCollider Server Synth Engine Command Reference
categories:: Server>Architecture

The following is a list of all server commands and their arguments.

Each command has a command number which can be sent to the server as a 32 bit integer instead of an OSC style string. Command numbers are listed at the end of this document.

If a command's description contains the word strong::Asynchronous::, then that command will be passed to a background thread to complete so as not to steal CPU time from the audio synthesis thread. All asynchronous commands send a reply to the client when they are completed. Many asynchronous commands can contain an OSC message or bundle to be executed upon completion. eg.
code::
    ["/d_load", "synthdefs/void.scsyndef",
        ["/s_new", "void", 1001, 1, 0] // completion message
    ]
::

section:: Master Controls
-}

{-|
subsection:: /quit

Quit program. Exits the synthesis server.

definitionlist::
## Asynchronous. || Replies to sender with strong::/done:: just before completion.
::
-}

quit :: OSC
quit = OSC "/quit" []

{-
subsection:: /notify
Register to receive notifications from server
table::
## strong::int:: || one to receive notifications, zero to stop receiving them.
::
If argument is one, server will remember your return address and send you notifications. if argument is zero, server will stop sending you notifications.

definitionlist::
## Asynchronous. || Replies to sender with strong::/done /notify clientID:: when complete. If this client has registered for notifications before, this may be the same ID. Otherwise it will be a new one. Clients can use this ID in multi-client situations to avoid conflicts in node IDs, bus indices, buffer numbers, etc.
::
-}

{-
subsection:: /status

Query the status. Replies to sender with the following message:
definitionlist::
## /status.reply || table::
## int || 1. unused.
## int || number of unit generators.
## int || number of synths.
## int || number of groups.
## int || number of loaded synth definitions.
## float || average percent CPU usage for signal processing
## float || peak percent CPU usage for signal processing
## double || nominal sample rate
## double || actual sample rate
::

note:: teletype::/status:: messages won't be posted, if the server is in teletype::/dumpOSC:: mode::

::
-}

{-
subsection:: /cmd
Plug-in defined command.
table::
## strong::string:: || command name
## strong::...:: || any arguments
::
Commands are defined by plug-ins.
-}

{-|
subsection:: /dumpOSC
Display incoming OSC messages.
table::
## strong::int:: || code
::
Turns on and off printing of the contents of incoming Open Sound Control messages. This is useful when debugging your command stream.
-}

{-
The values for the code are as follows:
table::
## 0 || turn dumping OFF.
## 1 || print the parsed contents of the message.
## 2 || print the contents in hexadecimal.
## 3 || print both the parsed and hexadecimal representations of the contents.
::
-}

data DumpOSCMode
   = DumpOSC_Off
   | DumpOSC_Parsed
   | DumpOSC_Hex
   | DumpOSC_ParsedAndHex
 deriving (Eq, Show, Read, Ord)

dumpOSC :: DumpOSCMode -> OSC
dumpOSC mode = OSC "/dumpOSC" [OSC_I modeInt]
 where
   modeInt = case mode of
      DumpOSC_Off -> 0
      DumpOSC_Parsed -> 1
      DumpOSC_Hex -> 2
      DumpOSC_ParsedAndHex -> 3

{-|
subsection:: /sync
Notify when async commands have completed.
table::
## strong::int:: || a unique number identifying this command.
::
Replies with a strong::/synced:: message when all asynchronous commands received before this one have completed. The reply will contain the sent unique ID.

definitionlist::
## Asynchronous. || Replies to sender with strong:: /synced, ID :: when complete.
::

-}

sync :: SyncId -> OSC
sync (SyncId syncId) = OSC "/sync" [OSC_I syncId]

{-|
subsection:: /clearSched

Clear all scheduled bundles. Removes all bundles from the scheduling queue.
-}

clearSched :: OSC
clearSched = OSC "/clearSched" []

{-
subsection:: /error
Enable/disable error message posting.
table::
## strong::int:: || mode
::
Turn on or off error messages sent to the SuperCollider post window. Useful when sending a message, such as strong::/n_free::, whose failure does not necessarily indicate anything wrong.

The values for mode are as follows:
table::
## 0 || turn off error posting until the next ['/error', 1] message.
## 1 || turn on error posting.
::
For convenience of client-side methods, you can also suppress errors temporarily, for the scope of a single bundle.
table::
## -1 || turn off locally in the bundle -- error posting reverts to the "permanent" setting for the next message or bundle.
## -2 || turn on locally in the bundle.
::
These "temporary" states accumulate within a single bundle -- so if you have nested calls to methods that use bundle-local error suppression, error posting remains off until all the layers have been unwrapped.
If you use ['/error', -1] within a self-bundling method, you should always close it with ['/error', -2] so that subsequent bundled messages will take the correct error posting status.
However, even if this is not done, the next bundle or message received will begin with the standard error posting status, as set by modes 0 or 1.

Temporary error suppression may not affect asynchronous commands in every case.
-}

{-
subsection:: /version

Query the SuperCollider version. Replies to sender with the following message:
definitionlist::
## /version.reply || table::
## string || Program name. May be "scsynth" or "supernova".
## int || Major version number. Equivalent to sclang's code::Main.scVersionMajor::.
## int || Minor version number. Equivalent to sclang's code::Main.scVersionMinor::.
## string || Patch version name. Equivalent to sclang's code::Main.scVersionPostfix::.
## string || Git branch name.
## string || First seven hex digits of the commit hash.
::
::

The standard human-readable version string can be constructed by concatenating code:: major_version ++ "." ++ minor_version ++ patch_version ::. Since version information is easily accessible to sclang users via the methods described above, this command is mostly useful for alternate clients.

The git branch name and commit hash could be anything if the user has forked SC, so they should only be used for display and user interface purposes.

section:: Synth Definition Commands
-}

{-|
subsection:: /d_recv
Receive a synth definition file.
table::
## strong::bytes:: || buffer of data.
## strong::bytes:: || an OSC message to execute upon completion. (optional)
::
Loads a file of synth definitions from a buffer in the message. Resident definitions with the same names are overwritten.

definitionlist::
## Asynchronous. || Replies to sender with strong::/done:: when complete.
::
-}

-- | Send Synth Definitions to the server.
-- 
--   The second argument is an optional message to execute on completion.
d_recv :: [LiteralSynthDef] -> Maybe OSC -> OSC
d_recv synthDefs = \case
   Nothing -> OSC "/d_recv" [OSC_B sdFile]
   -- Just c ->  OSC "/d_recv" [OSC_B sdFile, OSC_B (completionMsg c)]
   Just c ->  OSC "/d_recv" [OSC_B sdFile, OSC_B (encodeOSC c)]
 where
   sdFile :: ByteString
   sdFile = encodeSynthDefFile $ SynthDefFile synthDefs
{-
   completionMsg :: Either ByteString OSC -> ByteString
   completionMsg = \case
      Left bs -> bs
      Right osc -> encodeOSC osc
-}

{-|
subsection:: /d_load
Load synth definition.
table::
## strong::string:: || pathname of file. Can be a pattern like code:: "synthdefs/perc-*" ::
## strong::bytes:: || an OSC message to execute upon completion. (optional)
::
Loads a file of synth definitions. Resident definitions with the same names are overwritten.

definitionlist::
## Asynchronous. || Replies to sender with strong::/done:: when complete.
::
-}

d_load :: FilePath -> Maybe OSC -> OSC
d_load (OSC_S . UTF8.fromString -> path) = \case
   Nothing -> OSC "/d_load" [path]
   Just c ->  OSC "/d_load" [path, OSC_B (encodeOSC c)]

{-|
subsection:: /d_loadDir
Load a directory of synth definitions.
table::
## strong::string:: || pathname of directory.
## strong::bytes:: || an OSC message to execute upon completion. (optional)
::
Loads a directory of synth definitions files. Resident definitions with the same names are overwritten.

definitionlist::
## Asynchronous. || Replies to sender with strong::/done:: when complete.
::
-}

d_loadDir :: FilePath -> Maybe OSC -> OSC
d_loadDir (OSC_S . UTF8.fromString -> path) = \case
   Nothing -> OSC "/d_loadDir" [path]
   Just c ->  OSC "/d_loadDir" [path, OSC_B (encodeOSC c)]


{-
subsection:: /d_free
Delete synth definition.
table::
## N * strong::string:: || synth def name
::

Removes a synth definition. The definition is removed immediately, and does not wait for synth nodes based on that definition to end.

section:: Node Commands
-}

{-|
subsection:: /n_free
Delete a node.
table::
## N * strong::int:: || node ID
::

Stops a node abruptly, removes it from its group, and frees its memory. A list of node IDs may be specified. Using this method can cause a click if the node is not silent at the time it is freed.
-}

n_free :: NodeId -> OSC
n_free (NodeId nId) =
   OSC "/n_free" [OSC_I nId]

{-|
subsection:: /n_run
Turn node on or off.
table::
## N * || table::
    ## strong::int:: || node ID
    ## strong::int:: || run flag
    ::
::
list::
## If the run flag set to zero then the node will not be executed.
## If the run flag is set back to one, then it will be executed.
::
Using this method to start and stop nodes can cause a click if the node is not silent at the time run flag is toggled.
-}

n_run :: NodeId -> Bool -> OSC
n_run (NodeId nId) shouldRun =
   OSC "/n_run" [OSC_I nId, OSC_I $ case shouldRun of { True -> 1 ; False -> 0 }]

{-|
subsection:: /n_set
Set a node's control value(s).
table::
## strong::int:: || node ID
## N * || table::
    ## strong::int:: or strong::string:: || a control index or name
    ## strong::float:: or strong::int:: || a control value
    ::
::

Takes a list of pairs of control indices and values and sets the controls to those values. If the node is a group, then it sets the controls of every node in the group.

This message now supports array type tags ($[ and $]) in the control/value component of the OSC message.  Arrayed control values are applied in the manner of n_setn (i.e., sequentially starting at the indexed or named control).
-}

n_set :: NodeId -> [(String, Either Int32 Float)] -> OSC
n_set (NodeId nodeId) params =
   OSC "/n_set" $ (OSC_I nodeId :) $ (concat :: [[a]] -> [a])
      [ [OSC_S $ UTF8.fromString k, eitherOSC v] | (k, v) <- params ]

eitherOSC :: Either Int32 Float -> OSCDatum
eitherOSC = \case
   Left i -> OSC_I i
   Right f -> OSC_F f

{-
subsection:: /n_setn
Set ranges of a node's control value(s).
table::
## strong::int:: || node ID
## N * || table::
    ## strong::int:: or strong::string:: || a control index or name
    ## strong::int:: || number of sequential controls to change (M)
    ## M * strong::float:: or strong::int:: || control value(s)
    ::
::

Set contiguous ranges of control indices to sets of values. For each range, the starting control index is given followed by the number of controls to change, followed by the values. If the node is a group, then it sets the controls of every node in the group.
-}

{-
subsection:: /n_fill
Fill ranges of a node's control value(s).
table::
## strong::int:: || node ID
## N * || table::
    ## strong::int:: or strong::string:: || a control index or name
    ## strong::int:: || number of values to fill (M)
    ## strong::float:: or strong::int:: || value
    ::
::

Set contiguous ranges of control indices to single values. For each range, the starting control index is given followed by the number of controls to change, followed by the value to fill. If the node is a group, then it sets the controls of every node in the group.
-}

{-
subsection:: /n_map
Map a node's controls to read from a bus.
table::
## strong::int:: || node ID
## N * || table::
    ## strong::int:: or strong::string:: || a control index or name
    ## strong::int:: || control bus index
    ::
::

Takes a list of pairs of control names or indices and bus indices and causes those controls to be read continuously from a global control bus. If the node is a group, then it maps the controls of every node in the group. If the control bus index is -1 then any current mapping is undone. Any n_set, n_setn and n_fill command will also unmap the control.
-}

{-
subsection:: /n_mapn
Map a node's controls to read from buses.
table::
## strong::int:: || node ID
## N * || table::
    ## strong::int:: or strong::string:: || a control index or name
    ## strong::int:: || control bus index
    ## strong::int:: || number of controls to map
    ::
::

Takes a list of triplets of control names or indices, bus indices, and number of controls to map and causes those controls to be mapped sequentially to buses. If the node is a group, then it maps the controls of every node in the group. If the control bus index is -1 then any current mapping is undone. Any n_set, n_setn and n_fill command will also unmap the control.
-}

{-
subsection:: /n_mapa
Map a node's controls to read from an audio bus.
table::
## strong::int:: || node ID
## N * || table::
    ## strong::int:: or strong::string:: || a control index or name
    ## strong::int:: || control bus index
    ::
::

Takes a list of pairs of control names or indices and audio bus indices and causes those controls to be read continuously from a global audio bus. If the node is a group, then it maps the controls of every node in the group. If the audio bus index is -1 then any current mapping is undone. Any n_set, n_setn and n_fill command will also unmap the control. For the full audio rate signal, the argument must have its rate set to \ar.
-}

{-
subsection:: /n_mapan
Map a node's controls to read from audio buses.
table::
## strong:: int:: || node ID
## N * || table::
    ## strong::int:: or strong::string:: || a control index or name
    ## strong::int:: || control bus index
    ## strong::int:: || number of controls to map
    ::
::

Takes a list of triplets of control names or indices, audio bus indices, and number of controls to map and causes those controls to be mapped sequentially to buses. If the node is a group, then it maps the controls of every node in the group. If the audio bus index is -1 then any current mapping is undone. Any n_set, n_setn and n_fill command will also unmap the control. For the full audio rate signal, the argument must have its rate set to \ar.
-}

{-
subsection:: /n_before
Place a node before another.
table::
## N * || table::
    ## strong::int:: || the ID of the node to place (A)
    ## strong::int:: || the ID of the node before which the above is placed (B)
    ::
::
Places node A in the same group as node B, to execute immediately before node B.
-}

{-
subsection:: /n_after
Place a node after another.
table::
## N * || table::
    ## strong::int:: || the ID of the node to place (A)
    ## strong::int:: || the ID of the node after which the above is placed (B)
    ::
::
Places node A in the same group as node B, to execute immediately after node B.
-}

{-
subsection:: /n_query
Get info about a node.
table::
## N * strong::int:: || node ID
::
The server sends an /n_info message for each node to registered clients.
See Node Notifications below for the format of the /n_info message.
-}

{-
subsection:: /n_trace
Trace a node.
table::
## N * strong::int:: || node IDs
::

Causes a synth to print out the values of the inputs and outputs of its unit generators for one control period. Causes a group to print the node IDs and names of each node in the group for one control period.
-}

{-
subsection:: /n_order
Move and order a list of nodes.
table::
## strong::int:: || add action (0,1,2 or 3 see below)
## strong::int:: || add target ID
## N * strong::int:: || node IDs
::

Move the listed nodes to the location specified by the target and add action, and place them in the order specified. Nodes which have already been freed will be ignored.
definitionlist::
## add actions: || table::
    ## 0 || construct the node order at the head of the group specified by the add target ID.
    ## 1 || construct the node order at the tail of the group specified by the add target ID.
    ## 2 || construct the node order just before the node specified by the add target ID.
    ## 3 || construct the node order  just after the node specified by the add target ID.
    ::
::
-}

{-|
section:: Synth Commands

subsection:: /s_new
Create a new synth.
table::
## strong::string:: || synth definition name
## strong::int:: || synth ID
## strong::int:: || add action (0,1,2, 3 or 4 see below)
## strong::int:: || add target ID
## N * || table::
    ## strong::int:: or strong::string:: || a control index or name
	## strong::float:: or strong::int:: or strong::string::|| floating point and integer arguments are interpreted as control value.
                                                              a symbol argument consisting of the letter 'c' or 'a' (for control or audio) followed by the bus's index.
    ::
::

Create a new synth from a synth definition, give it an ID, and add it to the tree of nodes. There are four ways to add the node to the tree as determined by the add action argument which is defined as follows:
definitionlist::
## add actions: || table::
    ## 0 || add the new node to the the head of the group specified by the add target ID.
    ## 1 || add the new node to the the tail of the group specified by the add target ID.
    ## 2 || add the new node just before the node specified by the add target ID.
    ## 3 || add the new node just after the node specified by the add target ID.
    ## 4 || the new node replaces the node specified by the add target ID. The target node is freed.
    ::
::
-}

addActionInt :: Integral i => AddAction -> i
addActionInt = \case
   AddToHead -> 0
   AddToTail -> 1
   AddBefore -> 2
   AddAfter -> 3
   ReplaceNode -> 4

data AddAction
   = AddToHead
   | AddToTail
   | AddBefore
   | AddAfter
   | ReplaceNode
 deriving (Eq, Ord, Show)

{-
Controls may be set when creating the synth. The control arguments are the same as for the n_set command.

If you send strong::/s_new:: with a synth ID of -1, then the server will generate an ID for you. The server reserves all negative IDs. Since you don't know what the ID is, you cannot talk to this node directly later. So this is useful for nodes that are of finite duration and that get the control information they need from arguments and buses or messages directed to their group. In addition no notifications are sent when there are changes of state for this node, such as strong::/go::, strong::/end::, strong::/on::, strong::/off::.

If you use a node ID of -1 for any other command, such as strong::/n_map::, then it refers to the most recently created node by strong::/s_new:: (auto generated ID or not). This is how you can map  the controls of a node with an auto generated ID. In a multi-client situation, the only way you can be sure what node -1 refers to is to put the messages in a bundle.

This message now supports array type tags ($[ and $]) in the control/value component of the OSC message.  Arrayed control values are applied in the manner of n_setn (i.e., sequentially starting at the indexed or named control). See the link::Guides/NodeMessaging:: helpfile.
-}

s_new :: ByteString -> NodeId -> AddAction -> NodeId -> [(ByteString, Either Int32 Float)] -> OSC
s_new synthDefName (NodeId synthId) addAction (NodeId targetId) params =
   OSC "/s_new" $ [
        OSC_S synthDefName
      , OSC_I synthId
      , OSC_I $ addActionInt addAction
      , OSC_I targetId
      ] ++ concat [ [ OSC_S k, eitherOSC v ] | (k, v) <- params ]

{-
subsection:: /s_get
Get control value(s).
table::
## strong::int:: || synth ID
## N * strong::int:: or strong::string:: || a control index or name
::

Replies to sender with the corresponding strong::/n_set:: command.

subsection:: /s_getn
Get ranges of control value(s).
table::
## strong::int:: || synth ID
## N * || table::
    ## strong::int:: or strong::string:: || a control index or name
    ## strong::int:: || number of sequential controls to get (M)
    ::
::
Get contiguous ranges of controls. Replies to sender with the corresponding strong::/n_setn:: command.

subsection:: /s_noid
Auto-reassign synth's ID to a reserved value.
table::
## N * strong::int:: || synth IDs
::

This command is used when the client no longer needs to communicate with the synth and wants to have the freedom to reuse the ID. The server will reassign this synth to a reserved negative number. This command is purely for bookkeeping convenience of the client. No notification is sent when this occurs.
-}


{-|
section:: Group Commands

subsection:: /g_new
Create a new group.
table::
## N * || table::
    ## strong::int:: || new group ID
    ## strong::int:: || add action (0,1,2, 3 or 4 see below)
    ## strong::int:: || add target ID
    ::
::

Create a new group and add it to the tree of nodes.
There are four ways to add the group to the tree as determined by the add action argument which is defined as follows (the same as for strong::/s_new::):
definitionlist::
## add actions: || table::
    ## 0 || add the new group to the the head of the group specified by the add target ID.
    ## 1 || add the new group to the the tail of the group specified by the add target ID.
    ## 2 || add the new group just before the node specified by the add target ID.
    ## 3 || add the new group just after the node specified by the add target ID.
    ## 4 || the new node replaces the node specified by the add target ID. The target node is freed.
    ::
::
Multiple groups may be created in one command by adding arguments.

-}

g_new :: NodeId -> AddAction -> NodeId -> OSC
g_new (NodeId nodeId) addAction (NodeId targetNodeId) =
   OSC "/g_new" [
        OSC_I nodeId
      , OSC_I $ addActionInt addAction
      , OSC_I targetNodeId
      ]

{-|
subsection:: /p_new
Create a new parallel group.
table::
## N * || table::
    ## strong::int:: || new group ID
    ## strong::int:: || add action (0,1,2, 3 or 4 see below)
    ## strong::int:: || add target ID
    ::
::
Create a new parallel group and add it to the tree of nodes. Parallel groups are relaxed groups, their child nodes are evaluated in unspecified order.
There are four ways to add the group to the tree as determined by the add action argument which is defined as follows (the same as for strong::/s_new::):
definitionlist::
## add actions: || table::
    ## 0 || add the new group to the the head of the group specified by the add target ID.
    ## 1 || add the new group to the the tail of the group specified by the add target ID.
    ## 2 || add the new group just before the node specified by the add target ID.
    ## 3 || add the new group just after the node specified by the add target ID.
    ## 4 || the new node replaces the node specified by the add target ID. The target node is freed.
    ::
::
Multiple groups may be created in one command by adding arguments.
-}

-- Can dedupe with g_new:
p_new :: NodeId -> AddAction -> NodeId -> OSC
p_new (NodeId nodeId) addAction (NodeId targetNodeId) =
   OSC "/p_new" [
        OSC_I nodeId
      , OSC_I $ addActionInt addAction
      , OSC_I targetNodeId
      ]


{-
subsection:: /g_head
Add node to head of group.
table::
## N * || table::
    ## strong::int:: || group ID
    ## strong::int:: || node ID
    ::
::

Adds the node to the head (first to be executed) of the group.

subsection:: /g_tail
Add node to tail of group.
table::
## N * || table::
    ## strong::int:: || group ID
    ## strong::int:: || node ID
    ::
::
Adds the node to the tail (last to be executed) of the group.
-}

{-|
subsection:: /g_freeAll
Delete all nodes in a group.
table::
## N * strong::int:: || group ID(s)
::
Frees all nodes in the group. A list of groups may be specified.
-}

g_freeAll :: [NodeId] -> OSC
g_freeAll groupsToFree =
   OSC "/g_freeAll" [ OSC_I n | (NodeId n) <- groupsToFree ]

{-
subsection:: /g_deepFree
Free all synths in this group and all its sub-groups.
table::
## N * strong::int:: || group ID(s)
::
Traverses all groups below this group and frees all the synths. Sub-groups are not freed. A list of groups may be specified.

subsection:: /g_dumpTree
Post a representation of this group's node subtree.
table::
## N * || table::
    ## strong::int:: || group ID
    ## strong::int:: || flag; if not 0 the current control (arg) values for synths will be posted
    ::
::

Posts a representation of this group's node subtree, i.e. all the groups and synths contained within it, optionally including the current control values for synths.

subsection:: /g_queryTree
Get a representation of this group's node subtree.
table::
## N * || table::
    ## strong::int:: || group ID
    ## strong::int:: || flag: if not 0 the current control (arg) values for synths will be included
    ::
::
Request a representation of this group's node subtree, i.e. all the groups and synths contained within it. Replies to the sender with a strong::/g_queryTree.reply:: message listing all of the nodes contained within the group in the following format:
table::
## strong::int:: || flag: if synth control values are included 1, else 0
## strong::int:: || node ID of the requested group
## strong::int:: || number of child nodes contained within the requested group
## then for each node in the subtree: || table::
    ## strong::int:: || node ID
    ## strong::int:: || number of child nodes contained within this node. If -1 this is a synth, if >=0 it's a group
    ## then, if this node is a synth:
    ## strong::symbol:: || the SynthDef name for this node.
	## then, if flag (see above) is true:
    ## strong::int:: || numControls for this synth (M)
    ## M * || table::
        ## strong::symbol:: or strong::int:: || control name or index
        ## strong::float:: or strong::symbol:: || value or control bus mapping symbol (e.g. 'c1')
        ::
    ::
::

N.B. The order of nodes corresponds to their execution order on the server. Thus child nodes (those contained within a group) are listed immediately following their parent. See the method Server:queryAllNodes for an example of how to process this reply.

section:: Unit Generator Commands

subsection:: /u_cmd
Send a command to a unit generator.
table::
## strong::int:: || node ID
## strong::int:: || unit generator index
## strong::string:: || command name
## strong::...:: || any arguments
::
Sends all arguments following the command name to the unit generator to be performed. Commands are defined by unit generator plug ins.
-}

{-|
section:: Buffer Commands

Buffers are stored in a global array, indexed by integers starting at zero.

subsection:: /b_alloc
Allocate buffer space.
table::
## strong::int:: || buffer number
## strong::int:: || number of frames
## strong::int:: || number of channels (optional. default = 1 channel)
## strong::bytes:: || an OSC message to execute upon completion. (optional)
::
Allocates zero filled buffer to number of channels and samples.
definitionlist::
## Asynchronous. || Replies to sender with strong::/done /b_alloc bufNum:: when complete.
::
-}

-- | Second and third arguments are number of frames and number of channels,
--   respectively
b_alloc :: BufferId -> Int32 -> Int32 -> Maybe OSC -> OSC
b_alloc (BufferId bufId) numFrames numChans maybeComplete =
   OSC "/b_alloc" [
        OSC_I bufId
      , OSC_I numFrames
      , OSC_I numChans
      ] `addMaybeComplete` maybeComplete

addMaybeComplete :: OSC -> Maybe OSC -> OSC
addMaybeComplete (OSC path existingData) maybeComplete =
   OSC path $ existingData ++ case maybeComplete of
      Nothing -> []
      Just cmd -> [OSC_B (encodeOSC cmd)]

{-|
subsection:: /b_allocRead
Allocate buffer space and read a sound file.
table::
## strong::int:: || buffer number
## strong::string:: || path name of a sound file.
## strong::int:: || starting frame in file (optional. default = 0)
## strong::int:: || number of frames to read (optional. default = 0, see below)
## strong::bytes:: || an OSC message to execute upon completion. (optional)
::
Allocates buffer to number of channels of file and number of samples requested, or fewer if sound file is smaller than requested. Reads sound file data from the given starting frame in the file. If the number of frames argument is less than or equal to zero, the entire file is read.
definitionlist::
## Asynchronous. || Replies to sender with strong::/done /b_allocRead bufNum:: when complete.
::
-}

-- | The first Int32 is the starting frame to read (often this is zero);
--   the second is an optional number of frames to read ('Nothing' means
--   the rest of the file is read)
b_allocRead :: BufferId -> FilePath -> Int32 -> Maybe Int32 -> Maybe OSC -> OSC
b_allocRead (BufferId bufId) fPath startFrame numFramesMay maybeComplete =
   OSC "/b_allocRead" [
        OSC_I bufId
          -- Note we need 'UTF8.fromString' here and not
          -- Data.ByteString.Char8.pack. This is because .Char8
          -- will pack as truncated UTF-16 or UTF-32, encoding
          -- e.g. the 'é' in Beyoncé as 0xE9 (truncated from
          -- 0x00E9), when in fact we want 0xC3 0xA9:
      , OSC_S (UTF8.fromString fPath)
      , OSC_I startFrame
      , OSC_I $ fromMaybe (-1) numFramesMay
      ] `addMaybeComplete` maybeComplete


{-
subsection:: /b_allocReadChannel
Allocate buffer space and read channels from a sound file.
table::
## strong::int:: || buffer number
## strong::string:: || path name of a sound file
## strong::int:: || starting frame in file
## strong::int:: || number of frames to read
## N * || N >= 0
    table::
    ## strong::int:: || source file channel index
    ::
## strong::bytes:: || an OSC message to execute upon completion. (optional)
::

As b_allocRead, but reads individual channels into the allocated buffer in the order specified.
definitionlist::
## Asynchronous. || Replies to sender with strong::/done /b_allocReadChannel bufNum:: when complete.
::


subsection:: /b_read
Read sound file data into an existing buffer.
table::
## strong::int:: || buffer number
## strong::string:: || path name of a sound file.
## strong::int:: || starting frame in file (optional. default = 0)
## strong::int:: || number of frames to read (optional. default = -1, see below)
## strong::int:: || starting frame in buffer (optional. default = 0)
## strong::int:: || leave file open (optional. default = 0)
## strong::bytes:: || an OSC message to execute upon completion. (optional)
::
Reads sound file data from the given starting frame in the file and writes it to the given starting frame in the buffer. If number of frames is less than zero, the entire file is read.
If reading a file to be used by link::Classes/DiskIn:: ugen then you will want to set "leave file open" to one, otherwise set it to zero.
definitionlist::
## Asynchronous. || Replies to sender with strong::/done /b_read bufNum:: when complete.
::


subsection:: /b_readChannel
Read sound file channel data into an existing buffer.
table::
## strong::int:: || buffer number
## strong::string:: || path name of a sound file
## strong::int:: || starting frame in file
## strong::int:: || number of frames to read
## strong::int:: || starting frame in buffer
## strong::int:: || leave file open
## N * || N >= 0
    table::
    ## strong::int:: || source file channel index
    ::
## strong::bytes:: || completion message
::

As strong::b_read::, but reads individual channels in the order specified. The number of channels requested must match the number of channels in the buffer.
definitionlist::
## Asynchronous. || Replies to sender with strong::/done /b_readChannel bufNum:: when complete.
::
-}


{-|
subsection:: /b_write
Write sound file data.
table::
## strong::int:: || buffer number
## strong::string:: || path name of a sound file.
## strong::string:: || header format.
## strong::string:: || sample format.
## strong::int:: || number of frames to write (optional. default = -1, see below)
## strong::int:: || starting frame in buffer (optional. default = 0)
## strong::int:: || leave file open (optional. default = 0)
## strong::bytes:: || an OSC message to execute upon completion. (optional)
::

Write a buffer as a sound file.
definitionlist::
## Header format is one of: || "aiff", "next", "wav", "ircam"", "raw"
## Sample format is one of: || "int8", "int16", "int24", "int32", "float", "double", "mulaw", "alaw"
::
Not all combinations of header format and sample format are possible.
If number of frames is less than zero, all samples from the starting frame to the end of the buffer are written.
If opening a file to be used by DiskOut ugen then you will want to set "leave file open" to one, otherwise set it to zero. If "leave file open" is set to one then the file is created, but no frames are written until the DiskOut ugen does so.
definitionlist::
## Asynchronous. || Replies to sender with strong::/done /b_write bufNum:: when complete.
::
-}

-- | First String is header format (e.g. "wav"), second String is sample format
--   (e.g. "float"). The (Maybe Int32) is number of frames to write ('Nothing'
--   writes to the end of the buffer), the Int32 that follows it is the start
--   frame (which is often zero). The Bool is whether to leave the file open.
--
--   More details (e.g. the available header and sample formats) are available
--   in the SuperCollider docs.
b_write :: BufferId -> FilePath -> String -> String -> Maybe Int32 -> Int32 -> Bool -> Maybe OSC -> OSC
b_write (BufferId bufId) fPath headerFmt sampleFmt numFramesMay startFrame keepOpen completionMaybe =
   OSC "/b_write" [
        OSC_I bufId
      , OSC_S $ UTF8.fromString fPath
      , OSC_S $ UTF8.fromString headerFmt
      , OSC_S $ UTF8.fromString sampleFmt
      , OSC_I $ fromMaybe (-1) numFramesMay
      , OSC_I startFrame
      , OSC_I $ case keepOpen of { True -> 1 ; False -> 0 }
      ] `addMaybeComplete` completionMaybe

{-|
subsection:: /b_free
Free buffer data.
table::
## strong::int:: || buffer number
## strong::bytes:: || an OSC message to execute upon completion. (optional)
::
Frees buffer space allocated for this buffer.
definitionlist::
## Asynchronous. || Replies to sender with strong::/done /b_free bufNum:: when complete.
::
-}

b_free :: BufferId -> Maybe OSC -> OSC
b_free (BufferId bufId) completionMaybe =
   OSC "/b_free" [
        OSC_I bufId
      ] `addMaybeComplete` completionMaybe

{-|
subsection:: /b_zero
Zero sample data.
table::
## strong::int:: || buffer number
## strong::bytes:: || an OSC message to execute upon completion. (optional)
::
Sets all samples in the buffer to zero.
definitionlist::
## Asynchronous. || Replies to sender with strong::/done /b_zero bufNum:: when complete.
::
-}

b_zero :: BufferId -> Maybe OSC -> OSC
b_zero (BufferId bufferId) maybeComplete =
   OSC "/b_zero" [OSC_I bufferId]
      `addMaybeComplete` maybeComplete

{-
subsection:: /b_set
Set sample value(s).
table::
## strong::int:: || buffer number
## N * || table::
    ## strong::int:: || a sample index
    ## strong::float:: || a sample value
    ::
::
Takes a list of pairs of sample indices and values and sets the samples to those values.

subsection:: /b_setn
Set ranges of sample value(s).
table::
## strong::int:: || buffer number
## N * || table::
    ## strong::int:: || sample starting index
    ## strong::int:: || number of sequential samples to change (M)
    ## M * strong::float:: || a sample value
    ::
::
Set contiguous ranges of sample indices to sets of values. For each range, the starting sample index is given followed by the number of samples to change, followed by the values.

subsection:: /b_fill
Fill ranges of sample value(s).
table::
## strong::int:: || buffer number
## N * || table::
    ## strong::int:: || sample starting index
    ## strong::int:: || number of samples to fill (M)
    ## strong::float:: || value
    ::
::
Set contiguous ranges of sample indices to single values. For each range, the starting sample index is given followed by the number of samples to change, followed by the value to fill. This is only meant for setting a few samples, not whole buffers or large sections.


subsection:: /b_gen
Call a command to fill a buffer.
table::
## strong::int:: || buffer number
## strong::string:: || command name
## strong::...:: || command arguments
::
Plug-ins can define commands that operate on buffers. The arguments after the command name are defined by the command. The currently defined link::Reference/Server-Command-Reference#Buffer Fill Commands#buffer fill commands:: are listed below in a separate section.

code::/b_gen:: does not accept a completion message as the final argument.
definitionlist::
## Asynchronous. || Replies to sender with strong::/done /b_gen bufNum:: when complete.
::
-}

{-|
subsection:: /b_close
Close soundfile.
table::
## strong::int:: || buffer number
## strong::bytes:: || an OSC message to execute upon completion. (optional)
::
After using a buffer with link::Classes/DiskOut::, close the soundfile and write header information.
definitionlist::
## Asynchronous. || Replies to sender with strong::/done /b_close bufNum:: when complete.
::
-}

b_close :: BufferId -> Maybe OSC -> OSC
b_close (BufferId bufId) completionMaybe =
   OSC "/b_close" [
        OSC_I bufId
      ] `addMaybeComplete` completionMaybe

{-|
subsection:: /b_query
Get buffer info.
table::
## N * strong::int:: || buffer number(s)
::

Responds to the sender with a strong::/b_info:: message.  The arguments to /b_info are as follows:
table::
## N * || table::
    ## strong::int:: || buffer number
    ## strong::int:: || number of frames
    ## strong::int:: || number of channels
    ## strong::float:: || sample rate
    ::
::

Since: vivid-0.4.1
-}

b_query :: [BufferId] -> OSC
b_query bufIds =
   OSC "/b_query" [ OSC_I bufId | (BufferId bufId) <- bufIds ]


{-
subsection:: /b_get
Get sample value(s).
table::
## strong::int:: || buffer number
## N * strong::int:: || a sample index
::

Replies to sender with the corresponding strong::/b_set:: command.

subsection:: /b_getn
Get ranges of sample value(s).
table::
## strong::int:: || buffer number
## N * || table::
    ## strong::int:: || starting sample index
    ## strong::int:: || number of sequential samples to get (M)
    ::
::

Get contiguous ranges of samples. Replies to sender with the corresponding strong::/b_setn:: command. This is only meant for getting a few samples, not whole buffers or large sections.


section:: Control Bus Commands

subsection:: /c_set
Set bus value(s).
table::
## N * || table::
    ## strong::int:: || a bus index
    ## strong::float:: or strong::int:: || a control value
    ::
::
Takes a list of pairs of bus indices and values and sets the buses to those values.

subsection:: /c_setn
Set ranges of bus value(s).
table::
## N * || table::
    ## strong::int:: || starting bus index
    ## strong::int:: || number of sequential buses to change (M)
    ## M * || table::
        ## strong::float:: or strong::int:: || a control value
        ::
    ::
::
Set contiguous ranges of buses to sets of values. For each range, the starting bus index is given followed by the number of channels to change, followed by the values.

subsection:: /c_fill
Fill ranges of bus value(s).
table::
## N * || table::
    ## strong::int:: || starting bus index
    ## strong::int:: || number of buses to fill (M)
    ## strong::float:: or strong::int:: || value
    ::
::
Set contiguous ranges of buses to single values. For each range, the starting sample index is given followed by the number of buses to change, followed by the value to fill.

subsection:: /c_get
Get bus value(s).
table::
## N * strong::int:: || a bus index
::
Takes a list of buses and replies to sender with the corresponding strong::/c_set:: command.

subsection:: /c_getn
Get ranges of bus value(s).
table::
## N * || table::
    ## strong::int:: || starting bus index
    ## strong::int:: || number of sequential buses to get (M)
    ::
::
Get contiguous ranges of buses. Replies to sender with the corresponding strong::/c_setn:: command.


section:: Non Real Time Mode Commands

subsection:: /nrt_end
End real time mode, close file.  Not yet implemented.

This message should be sent in a bundle in non real time mode.
The bundle timestamp will establish the ending time of the file.
This command will end non real time mode and close the sound file.
Replies to sender with strong::/done:: when complete.


section:: Replies to Commands

These messages are sent by the server in response to some commands.

subsection:: /done
An asynchronous message has completed.
table::
## strong::string:: || the name of the command
## strong::other:: || (optional) some commands provide other information, for example a buffer index.
::
Sent in response to all asynchronous commands. Sent only to the sender of the original message.

subsection:: /fail
An error occurred.
table::
## strong::string:: || the name of the command
## strong::string:: || the error message.
## strong::other:: || (optional) some commands provide other information, for example a buffer index.
::
There was a problem. Sent only to the sender of the original message.

subsection:: /late
A command was received too late.
not yet implemented
table::
## strong::int:: || the high 32 bits of the original time stamp.
## strong::int:: || the low 32 bits of the original time stamp.
## strong::int:: || the high 32 bits of the time it was executed.
## strong::int:: || the low 32 bits of the time it was executed.
::
The command was received too late to be executed on time. Sent only to the sender of the original message.

section:: Node Notifications from Server

These messages are sent as notification of some event to all clients who have registered via the strong::/notify:: command.

All of these have the same arguments:
table::
## strong::int:: || node ID
## strong::int:: || the node's parent group ID
## strong::int:: || previous node ID, -1 if no previous node.
## strong::int:: || next node ID, -1 if no next node.
## strong::int:: || 1 if the node is a group, 0 if it is a synth
## The following two arguments are only sent if the node is a group:
## strong::int:: || the ID of the head node, -1 if there is no head node.
## strong::int:: || the ID of the tail node, -1 if there is no tail node.
::

subsection:: /n_go
A node was started.
This command is sent to all registered clients when a node is created.

subsection:: /n_end
A node ended.
This command is sent to all registered clients when a node ends and is deallocated.

subsection:: /n_off
A node was turned off.
This command is sent to all registered clients when a node is turned off.

subsection:: /n_on
A node was turned on.
This command is sent to all registered clients when a node is turned on.

subsection:: /n_move
A node was moved.
This command is sent to all registered clients when a node is moved.

subsection:: /n_info
Reply to /n_query.
This command is sent to all registered clients in response to an strong::/n_query:: command.


section:: Trigger Notification
These messages are sent as notification of some event to all clients who have registered via the strong::/notify:: command.

subsection:: /tr
A trigger message.
table::
## strong::int:: || node ID
## strong::int:: || trigger ID
## strong::float:: || trigger value
::
This command is the mechanism that synths can use to trigger events in clients.
The node ID is the node that is sending the trigger. The trigger ID and value are determined by inputs to the SendTrig unit generator which is the originator of this message.


section:: Buffer Fill Commands

These are the currently defined fill routines for use with the strong::/b_gen:: command.

subsection:: Wave Fill Commands

There are three defined fill routines for sine waves.

The flags are defined as follows:
table::
## 1 || normalize - Normalize peak amplitude of wave to 1.0.
## 2 || wavetable - If set, then the buffer is written in wavetable format so that it can be read by interpolating oscillators.
## 4 || clear - if set then the buffer is cleared before new partials are written into it. Otherwise the new partials are summed with the existing contents of the buffer.
::

These flags can be added together to create a unique single integer flag that describes the true/false combinations for these three options:

table::
## 3 || 1 + 2 || normalize + wavetable
## 5 || 1 + 4 || normalize + clear
## 6 || 2 + 4 || wavetable + clear
## 7 || 1 + 2 + 4 || normalize + wavetable + clear
::

definitionlist::
## sine1 ||
table::
## strong::int:: || flags, see above
## N * || table::
    ## strong::float:: || partial amplitude
    ::
::
Fills a buffer with a series of sine wave partials. The first float value specifies the amplitude of the first partial, the second float value specifies the amplitude of the second partial, and so on.

## sine2 ||
table::
## strong::int:: || flags, see above
## N * || table::
    ## strong::float:: || partial frequency (in cycles per buffer)
    ## strong::float:: || partial amplitude
    ::
::
Similar to sine1 except that each partial frequency is specified explicitly instead of being an integer series of partials. Non-integer partial frequencies are possible.

## sine3 ||
table::
## strong::int:: || flags, see above
## N * || table::
    ## strong::float:: || partial frequency (in cycles per buffer)
    ## strong::float:: || partial amplitude
    ## strong::float:: || partial phase
    ::
::
Similar to sine2 except that each partial may have a nonzero starting phase.

## cheby ||
table::
## strong::int:: || flags, see above
## N * || table::
    ## strong::float:: || amplitude
    ::
::
Fills a buffer with a series of chebyshev polynomials, which can be defined as:
code::
cheby(n) = amplitude * cos(n * acos(x))
::
The first float value specifies the amplitude for n = 1, the second float value specifies the amplitude for n = 2, and so on. To eliminate a DC offset when used as a waveshaper, the wavetable is offset so that the center value is zero.
::

subsection:: Other Commands

definitionlist::
## copy ||
table::
## strong::int:: || sample position in destination
## strong::int:: || source buffer number
## strong::int:: || sample position in source
## strong::int:: || number of samples to copy
::
Copy samples from the source buffer to the destination buffer specified in the b_gen command. If the number of samples to copy is negative, the maximum number of samples possible is copied.
definitionlist::
## Asynchronous. || Replies to sender with strong::/done:: when complete.
::

::

section:: Command Numbers

These are the currently defined command numbers. More may be added to the end of the list in the future.
code::
enum {
    cmd_none = 0,

    cmd_notify = 1,
    cmd_status = 2,
    cmd_quit = 3,
    cmd_cmd = 4,

    cmd_d_recv = 5,
    cmd_d_load = 6,
    cmd_d_loadDir = 7,
    cmd_d_freeAll = 8,

    cmd_s_new = 9,

    cmd_n_trace = 10,
    cmd_n_free = 11,
    cmd_n_run = 12,
    cmd_n_cmd = 13,
    cmd_n_map = 14,
    cmd_n_set = 15,
    cmd_n_setn = 16,
    cmd_n_fill = 17,
    cmd_n_before = 18,
    cmd_n_after = 19,

    cmd_u_cmd = 20,

    cmd_g_new = 21,
    cmd_g_head = 22,
    cmd_g_tail = 23,
    cmd_g_freeAll = 24,

    cmd_c_set = 25,
    cmd_c_setn = 26,
    cmd_c_fill = 27,

    cmd_b_alloc = 28,
    cmd_b_allocRead = 29,
    cmd_b_read = 30,
    cmd_b_write = 31,
    cmd_b_free = 32,
    cmd_b_close = 33,
    cmd_b_zero = 34,
    cmd_b_set = 35,
    cmd_b_setn = 36,
    cmd_b_fill = 37,
    cmd_b_gen = 38,

    cmd_dumpOSC = 39,

    cmd_c_get = 40,
    cmd_c_getn = 41,
    cmd_b_get = 42,
    cmd_b_getn = 43,
    cmd_s_get = 44,
    cmd_s_getn = 45,

    cmd_n_query = 46,
    cmd_b_query = 47,

    cmd_n_mapn = 48,
    cmd_s_noid = 49,

    cmd_g_deepFree = 50,
    cmd_clearSched = 51,

    cmd_sync = 52,

    cmd_d_free = 53,

    cmd_b_allocReadChannel = 54,
    cmd_b_readChannel = 55,

    cmd_g_dumpTree = 56,
    cmd_g_queryTree = 57,


    cmd_error = 58,

    cmd_s_newargs = 59,

    cmd_n_mapa = 60,
    cmd_n_mapan = 61,
    cmd_n_order = 62,

    cmd_p_new = 63,

    cmd_version = 64,

    NUMBER_OF_COMMANDS = 65
};
::

copyright © 2002 James McCartney
-
soft::converted to ScDoc format 2011 by Jonatan Liljedahl::
-}
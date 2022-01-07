namespace OpenEtw
open System

type EtlTraceEvent() =
    member val is64bit    = false      with get, set
    member val hookId     = 2u         with get, set
    member val threadId   = 4u         with get, set
    member val processId  = 4u         with get, set
    member val timestamp  = 0L         with get, set
    member val guid       = Guid.Empty with get, set
    member val kernelTime = 0u         with get, set
    member val userTime   = 0u         with get, set
    member val payload : byte array = [||] with get, set

    member x.HeaderSize = 0x30
    member x.Size with get() = x.HeaderSize + x.payload.Length
    member x.Clone() = x.MemberwiseClone() :?> EtlTraceEvent

    member private x.Common (w : Util.ISerializer) size =
        w.UInt32 "hookId"     x.get_hookId     x.set_hookId     // 4
        w.UInt32 "threadId"   x.get_threadId   x.set_threadId   // 8
        w.UInt32 "processId"  x.get_processId  x.set_processId  // C
        w.Int64  "timestamp"  x.get_timestamp  x.set_timestamp  // 10
        w.Guid   "guid"       x.get_guid       x.set_guid       // 18
        w.UInt32 "kernelTime" x.get_kernelTime x.set_kernelTime // 28
        w.UInt32 "userTime"   x.get_userTime   x.set_userTime   // 2c

        let payloadSize = (int size) - (int x.HeaderSize)
        if (payloadSize < 0) then failwith "Negative payload size"
        if (payloadSize > 0) then 
            w.ByteArrayHex "payload" x.get_payload x.set_payload payloadSize // 30

        let paddingBytes = Util.paddingBytes x.Size
        if (paddingBytes > 0) then
            w.RepeatU8 "padding" 0uy paddingBytes

    member x.Serialize (w:Util.ISerializer) =
        let headerType = if x.is64bit then EtlHeaderType.FullHeader64 else EtlHeaderType.FullHeader32
        if (x.Size > int UInt16.MaxValue) then failwith "Payload too large"

        w.Comment "Trace Event"
        Util.writeValue w.UInt16 "size" (uint16 x.Size) // 0
        Util.writeValue w.EnumU16 "headerType" headerType EtlHeaderType.info // 2
        x.Common w x.Size

    static member Deserialize (w : Util.ISerializer) (size : uint16) headerType =
        let x = EtlTraceEvent()

        match headerType with
        | EtlHeaderType.FullHeader64 -> x.is64bit <- true
        | EtlHeaderType.FullHeader32 -> x.is64bit <- false
        | _ -> failwith "Invalid header type for trace event"

        x.Common w (int size)
        x

namespace OpenEtw
open System
open SerdesNet

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

    member private x.Common (s : ISerializer) size =
        x.hookId     <- s.UInt32("hookId",     x.hookId)     // 4
        x.threadId   <- s.UInt32("threadId",   x.threadId)   // 8
        x.processId  <- s.UInt32("processId",  x.processId)  // C
        x.timestamp  <- s.Int64 ("timestamp",  x.timestamp)  // 10
        x.guid       <- s.Guid  ("guid",       x.guid)       // 18
        x.kernelTime <- s.UInt32("kernelTime", x.kernelTime) // 28
        x.userTime   <- s.UInt32("userTime",   x.userTime)   // 2c

        let payloadSize = (int size) - (int x.HeaderSize)
        if (payloadSize < 0) then failwith "Negative payload size"
        if (payloadSize > 0) then 
            x.payload <- s.Bytes("payload", x.payload, payloadSize) // 30

        let paddingBytes = Util.paddingBytes x.Size
        if (paddingBytes > 0) then
            s.Pad("padding", paddingBytes, 0uy)

    member x.Serialize (s : ISerializer) =
        let headerType = if x.is64bit then EtlHeaderType.FullHeader64 else EtlHeaderType.FullHeader32
        if (x.Size > int UInt16.MaxValue) then failwith "Payload too large"

        s.Comment "Trace Event"
        s.UInt16("size", uint16 x.Size) |> ignore // 0
        s.EnumU16("headerType", headerType) |> ignore // 2
        x.Common s x.Size

    static member Deserialize (s : ISerializer) (size : uint16) headerType =
        let x = EtlTraceEvent()

        match headerType with
        | EtlHeaderType.FullHeader64 -> x.is64bit <- true
        | EtlHeaderType.FullHeader32 -> x.is64bit <- false
        | _ -> failwith "Invalid header type for trace event"

        x.Common s (int size)
        x

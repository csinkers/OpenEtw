namespace OpenEtw
open System
open SerdesNet

type EtlInstanceGuidEvent() =
    member val is64bit          = false      with get, set
    member val version          = 2u         with get, set
    member val threadId         = 4u         with get, set
    member val processId        = 4u         with get, set
    member val timestamp        = 0L         with get, set
    member val guid             = Guid.Empty with get, set
    member val kernelTime       = 0u         with get, set
    member val userTime         = 0u         with get, set
    member val instanceId       = 0u         with get, set
    member val parentInstanceId = 0u         with get, set
    member val parentGuid       = Guid.Empty with get, set
    member val payload : byte array = [||]   with get, set

    member x.HeaderSize with get() = 0x48
    member x.Size with get() = x.HeaderSize + x.payload.Length
    member x.Clone() = x.MemberwiseClone() :?> EtlInstanceGuidEvent

    member private x.Common (s : ISerializer) size =
        x.version          <- s.UInt32("version",          x.version)          // 4
        x.threadId         <- s.UInt32("threadId",         x.threadId)         // 8
        x.processId        <- s.UInt32("processId",        x.processId)        // C
        x.timestamp        <- s.Int64 ("timestamp",        x.timestamp)        // 10
        x.guid             <- s.Guid  ("guid",             x.guid)             // 18
        x.kernelTime       <- s.UInt32("kernelTime",       x.kernelTime)       // 28
        x.userTime         <- s.UInt32("userTime",         x.userTime)         // 2c
        x.instanceId       <- s.UInt32("instanceId",       x.instanceId)       // 30
        x.parentInstanceId <- s.UInt32("parentInstanceId", x.parentInstanceId) // 34
        x.parentGuid       <- s.Guid  ("parentGuid",       x.parentGuid)       // 38

        let payloadSize = (int size) - x.HeaderSize
        if(payloadSize < 0) then failwith "Negative payload size"
        if (payloadSize > 0) then 
            x.payload <- s.Bytes("payload", x.payload, payloadSize) // 48

        let paddingBytes = Util.paddingBytes x.Size
        if (paddingBytes > 0) then
            s.Pad("padding", paddingBytes, 0uy)

    member x.Serialize (s : ISerializer) =
        let headerType = if x.is64bit then EtlHeaderType.Instance64 else EtlHeaderType.Instance32
        if (x.Size > int UInt16.MaxValue) then failwith "Payload too large"

        s.Comment "Instance GUID Event"
        s.UInt16("size", uint16 x.Size) |> ignore // 0
        s.EnumU16("headerType", headerType) |> ignore  // 2
        x.Common s (uint16 x.Size)

    static member Deserialize (s : ISerializer) size headerType =
        let x = EtlInstanceGuidEvent()

        match headerType with
        | EtlHeaderType.Instance64 -> x.is64bit <- true
        | EtlHeaderType.Instance32 -> x.is64bit <- false
        | _ -> failwith "Invalid header type for instance event"

        x.Common s size
        x
        

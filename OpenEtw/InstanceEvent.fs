namespace OpenEtw
open System

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

    member private x.Common (w : Util.ISerializer) size =
        w.UInt32 "version"          x.get_version          x.set_version          // 4
        w.UInt32 "threadId"         x.get_threadId         x.set_threadId         // 8
        w.UInt32 "processId"        x.get_processId        x.set_processId        // C
        w.Int64  "timestamp"        x.get_timestamp        x.set_timestamp        // 10
        w.Guid   "guid"             x.get_guid             x.set_guid             // 18
        w.UInt32 "kernelTime"       x.get_kernelTime       x.set_kernelTime       // 28
        w.UInt32 "userTime"         x.get_userTime         x.set_userTime         // 2c
        w.UInt32 "instanceId"       x.get_instanceId       x.set_instanceId       // 30
        w.UInt32 "parentInstanceId" x.get_parentInstanceId x.set_parentInstanceId // 34
        w.Guid   "parentGuid"       x.get_parentGuid       x.set_parentGuid       // 38

        let payloadSize = (int size) - x.HeaderSize
        if(payloadSize < 0) then failwith "Negative payload size"
        if (payloadSize > 0) then 
            w.ByteArrayHex "payload" x.get_payload x.set_payload payloadSize // 48

        let paddingBytes = Util.paddingBytes x.Size
        if (paddingBytes > 0) then
            w.RepeatU8 "padding" 0uy paddingBytes

    member x.Serialize (w:Util.ISerializer) =
        let headerType = if x.is64bit then EtlHeaderType.Instance64 else EtlHeaderType.Instance32
        if (x.Size > int UInt16.MaxValue) then failwith "Payload too large"

        w.Comment "Instance GUID Event"
        Util.writeValue w.UInt16 "size" (uint16 x.Size) // 0
        Util.writeValue w.EnumU16 "headerType" headerType EtlHeaderType.info // 2
        x.Common w (uint16 x.Size)

    static member Deserialize (w : Util.ISerializer) size headerType =
        let x = EtlInstanceGuidEvent()

        match headerType with
        | EtlHeaderType.Instance64 -> x.is64bit <- true
        | EtlHeaderType.Instance32 -> x.is64bit <- false
        | _ -> failwith "Invalid header type for instance event"

        x.Common w size
        x
        

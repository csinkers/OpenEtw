module OpenEtw.Tests.BuildProvider
open OpenEtw

let build name =
    {EtwProvider.empty with
        className  = name
        name       = name
        symbol     = name
        guid       = System.Guid.NewGuid()
        levels     = []
        opcodes    = []
        keywords   = []
        channels   = []
        maps       = []
        tasks      = []
        events     = 
        [
            {
                id         = Some 0us
                name       = "Test"
                cppName    = "Test"
                symbol     = "Test"
                message    = None
                version    = 0uy
                task       = None
                level      = None
                channel    = None
                opcode     = None
                keywords   = []
                parameters = []
            }
        ]
    }

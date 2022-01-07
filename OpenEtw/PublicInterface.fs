namespace OpenEtw
open System
open FParsec

type Result<'a, 'err> =
    | Failure of 'err
    | Success of 'a

module Public =
    let parseHeader headerContent =
        let headerContentWithNewLine = 
            Environment.NewLine + // hack to allow preprocessor directives on the first line.
            headerContent

        match runParserOnString SourceHeaderParser.pSourceFile () "header" headerContentWithNewLine with
        | ParserResult.Failure (err, _, _) -> Failure err
        | ParserResult.Success (results, _, _) -> Success (ProcessTokenList.processTokens results)

    let generateManifest          = GenerateManifest.forProvider
    let generateCppSelfDescribing = GenerateCppSelfDescribing.forProvider
    let generateCsLegacy          = GenerateCsLegacy.forProvider
    let generateNodeWrapper       = GenerateNodePlugin.forProvider


//  Lin
//  Command line tool that analyzes text and links from files/urls!

//  Usage: lin [--extract-keywords|--extract-links|--categorize-links|--export-bookmark] [source]
//  The source can be a text/html file or url.


//  By @MATNESIS
//  2017/03/25 06:46 PM


//  To do
//      Links by category
//      Export links to categorized bookmark file
//      Css keywords as stop words
//      Google search source
//      Google query to decent url query
//      Bug when the last argument is not a source
//      Any json on the folder /stop will be applied as stop topWords
//      Bug with auth exception with https://nacion.com as source
//      Stop words by external file
//      Source Links depthness?
//      Improve words extraction (regex alternative?)
//      x File / URL source
//      x Links extraction
//      x Keywords extraction
//      x Command line parser
//      x Regex text extraction from html


open FSharp.Data
open System.Text.RegularExpressions
open System.Net



// ==== Constants ======

let header = """Command line tool that analyzes text and links from files/urls!
By @MATNESIS
"""

let usage =
    "Usage: lin [--extract-keywords|--extract-links|--categorize-links|--export-bookmark] [source]"



// ==== Command Line ======

module CommandLineParser =

    type CommandLineOptions = {
        source: string
        extractKeywords: bool
        extractLinks: bool
        categorizeLinks: bool
        exportLinksToCategorizedBookmark: bool }

    let rec parseRec args optionsSoFar =
        match args with
        | [] -> optionsSoFar
        | [last] -> parseRec [] { optionsSoFar with source = last }
        | "-ek" :: xs ->
            parseRec xs { optionsSoFar with extractKeywords = true }
        | "-el" :: xs ->
            parseRec xs { optionsSoFar with extractLinks = true }
        | "-cl" :: xs ->
            parseRec xs { optionsSoFar with categorizeLinks = true }
        | "-ecl" :: xs ->
            parseRec xs { optionsSoFar with exportLinksToCategorizedBookmark = true }
        | x :: xs ->
            printfn "lin: Illegal option '%s'" x
            printfn "%s" usage
            parseRec xs optionsSoFar

    let parse args =

        let defaultOptions = {
            source = "http://google.com?q=\"matnesis\"" // Me
            extractKeywords = false
            extractLinks = false
            categorizeLinks = false
            exportLinksToCategorizedBookmark = false }

        parseRec args defaultOptions



// ==== Text Manipulation ======

let extractText html =
    let pattern = "(?is)<!--.*?--\s*>|<script.*?</script>|<style.*?</style>"
    let html' = Regex(pattern, RegexOptions.Compiled).Replace(html, " ")
    let tagsFree = Regex("(?i)<[^>]*>", RegexOptions.Compiled).Replace(html', "  ")
                 |> WebUtility.HtmlDecode
    let tagsFree' = Regex.Replace(tagsFree, "\s{3,}", "  ")
    tagsFree'.ToLower()


let wordList text =
    [ for x in Regex("[^0-9\W]+").Matches(text)
        do yield x.Value ]


let rec removeWords text words =
    match words with
    | h :: t -> removeWords (Regex("\\b" + h + "\\b").Replace(text, " ")) t
    | [] -> text


// Returns a list of (word, count) tuples for any word in the text. Sorted by
// count, descendent.
let wordCount text words =
    [ for x in words do
        let count = Regex(@"\b" + Regex.Escape(x) + @"\b").Matches(text).Count
        if count > 1
        then yield (x, count) ]
    |> List.sortBy (fun (_, x) -> x)
    |> List.rev


let htmlToWords html ignore =
    let text = extractText html
    removeWords text ignore



// ==== HtmlDocument ======

let extractLinkSeq (htmlDoc: HtmlDocument) =
    htmlDoc.Descendants ["a"]
    |> Seq.choose (fun x ->
        x.TryGetAttribute("href")
        |> Option.map (fun y -> y.Value()))


let extractNameLinkSeq (htmlDoc: HtmlDocument) =
    htmlDoc.Descendants ["a"]
    |> Seq.choose (fun x ->
        x.TryGetAttribute("href")
        |> Option.map (fun y -> x.InnerText(), y.Value()))


// Returns a tuple list with the most used words and his count.
let topWords (htmlDoc: HtmlDocument) ignore =
    let words = htmlToWords (htmlDoc.ToString()) ignore
    let wordCount = wordCount words (wordList words)
    wordCount



// ==== Stop Words ======

type StopEs = JsonProvider<"/Users/andresv/Projects/lin/lin/stop-words_es.json">
type StopEn = JsonProvider<"/Users/andresv/Projects/lin/lin/stop-words_en.json">



// ==== Main ======

[<EntryPoint>]
let main argv =


    // Header when there aren't arguments
    if argv.Length < 1 then
        printfn "%s" header
        printfn "%s" usage


    // Parsing the the Command line
    let options = CommandLineParser.parse (List.ofArray argv) // i.e. ["--top-links"; "--export-bookmark"]


    // The source is the last argument
    let html = HtmlDocument.Load(options.source)


    // ==== Options Execution ======

    // Option: Extracting links
    if options.extractLinks then
        let links = extractLinkSeq html
        printfn ""
        links
            |> Seq.distinct
            |> Seq.iteri (fun i x ->  printfn "%i %s" i x)

    // Option: Extracting keywords
    if options.extractKeywords then
        let stopEs = List.ofArray <| (StopEs.GetSample()).Strings
        let stopEn = List.ofArray <| (StopEn.GetSample()).Strings
        let keywords = topWords html (stopEs @ stopEn)
        printfn ""
        keywords
            |> List.distinct
            |> List.iter (fun x -> printf "%s %d\t" <|| x)
        printfn ""


    0 // Exit code



// ==== Notes ======

//  Equivalent
//      let links = href |> Seq.map snd
//                       |> Seq.map (fun x -> snd x)

//  Prints a sequence of tuples
//      href |> Seq.iter (fun x -> printfn "%s %s" <|| x)

//  Returns all files and folders from files and folders.
//      let rec allFiles dirs =
//          match dirs with
//          | dirs when Seq.isEmpty dirs -> Seq.empty
//          | _ -> seq { yield! dirs |> Seq.collect Directory.EnumerateFiles
//                       yield! dirs |> Seq.collect Directory.EnumerateDirectories |> allFiles }

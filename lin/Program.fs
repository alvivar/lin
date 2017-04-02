
//  Lin
//  Command line tool that analyzes text and links!

//  Usage
//  lin [source] [--top-words|--extract-links|--categorize-links|--export-bookmark]]


//  By @MATNESIS
//  2017/03/25 06:46 PM


//  To do
//      Top words
//      Source
//          File
//          URL
//          Google search
//              Text query to obligatory decent url query
//          Links depthness
//      Analysis
//          Top words
//          Links by category
//              Export to categorized bookmark file


open FSharp.Data
open System.Text.RegularExpressions
open System.Net



// ------- Command Line Parser ---

module CommandLineParser =

    type CommandLineOptions = {
        topWords: bool
        categorizeLinks: bool
        extractLinks: bool
        exportLinksToCategorizedBookmark: bool }

    let rec parseCommandLineRec args optionsSoFar =
        match args with
        | [] -> optionsSoFar
        | "--top-words" :: xs ->
            parseCommandLineRec xs { optionsSoFar with topWords = true }
        | "--extract-links" :: xs ->
            parseCommandLineRec xs { optionsSoFar with extractLinks = true }
        | "--categorize-links" :: xs ->
            parseCommandLineRec xs { optionsSoFar with categorizeLinks = true }
        | "--export-bookmark" :: xs ->
            parseCommandLineRec xs { optionsSoFar with exportLinksToCategorizedBookmark = true }
        | x :: xs ->
            printfn "Option '%s' is unrecognized" x
            parseCommandLineRec xs optionsSoFar

    let parseCommandLine args =

        let defaultOptions = {
            topWords = false
            extractLinks = false
            categorizeLinks = false
            exportLinksToCategorizedBookmark = false }

        parseCommandLineRec args defaultOptions



// ------- Text Manipulation ---

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



// ------- HTML Analysis ---

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



// ------- Stop Words ---

type StopEs = JsonProvider<"/Users/andresv/Projects/lin/lin/stop-words_es.json">
type StopEn = JsonProvider<"/Users/andresv/Projects/lin/lin/stop-words_en.json">



// ------- Main ---

[<EntryPoint>]
let main argv =

    // Header
    printfn "%s" """LIN
Command line tool for quick text and links analysis
By @MATNESIS

Usage
lin [source] [--top-words|--extract-links|--categorize-links|--export-bookmark]
"""


    // Main Source
    let html = HtmlDocument.Load("https://www.kickstarter.com/projects/1068694633/narita-boy-the-retro-futuristic-pixel-game")


    // Options from the Command line
    let options = CommandLineParser.parseCommandLine (List.ofArray argv) // i.e. ["--top-links"; "--export-bookmark"]


    // -- Option -
    // Extracting links
    if options.extractLinks then
        let links = extractLinkSeq html
        printfn "Links extracted: "
        links |> Seq.iteri (fun i x ->  printf "(%i %s)" i x)


    // Stop words
    let stopEs = List.ofArray <| (StopEs.GetSample()).Strings
    let stopEn = List.ofArray <| (StopEn.GetSample()).Strings
    // stopEs.Strings |> List.iter (fun x -> printf "%s " x)


    // Top Words Option
    if options.topWords
    then
        // Keywords
        let words = htmlToWords (html.ToString()) (stopEs @ stopEn)
        let topKeywords = wordCount words (wordList words)

        topKeywords
            |> List.distinct
            |> List.iter (fun x -> printf "(%s, %d) " <|| x)


    0 // return an integer exit code



(* Learning +1 *)


// Equivalent
    // let links = href |> Seq.map snd
    //                  |> Seq.map (fun x -> snd x)

// Prints a sequence of tuples
    // href |> Seq.iter (fun x -> printfn "%s %s" <|| x)

// Returns all files and folders from files and folders.
// let rec allFiles dirs =
//     match dirs with
//     | dirs when Seq.isEmpty dirs -> Seq.empty
//     | _ -> seq { yield! dirs |> Seq.collect Directory.EnumerateFiles
//                  yield! dirs |> Seq.collect Directory.EnumerateDirectories |> allFiles }

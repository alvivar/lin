
// Lin
// Command line tool that analyzes text and links!

// By @MATNESIS
// 2017/03/25 06:46 PM


open FSharp.Data
open System.Text.RegularExpressions
open System.Net


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


// Returns a list of (word, count) tuples for any word in the text.
let wordCount text words =
    [ for x in words do
        let count = Regex(@"\b" + Regex.Escape(x) + @"\b").Matches(text).Count
        if count > 1
        then yield (x, count)
    ]
    |> List.sortBy (fun (_, x) -> x)
    |> List.rev


let htmlToWords html ignore =
    let text = extractText html
    removeWords text ignore


// Returns all files and folders from files and folders.
// let rec allFiles dirs =
//     match dirs with
//     | dirs when Seq.isEmpty dirs -> Seq.empty
//     | _ -> seq { yield! dirs |> Seq.collect Directory.EnumerateFiles
//                  yield! dirs |> Seq.collect Directory.EnumerateDirectories |> allFiles }


// Stop words
type StopEs = JsonProvider<"/Users/andresv/Projects/lin/lin/stop-words_es.json">
type StopEn = JsonProvider<"/Users/andresv/Projects/lin/lin/stop-words_en.json">


[<EntryPoint>]
let main argv =

    printfn "%A" argv

    // prinfn "%"


    let html = HtmlDocument.Load("https://news.ycombinator.com/best")

    // printfn "%s" (extractText (html.ToString()))


    // A tuple of names and links
    let href =
        html.Descendants ["a"]
        |> Seq.choose (fun x ->
            x.TryGetAttribute("href")
            |> Option.map (fun a -> x.InnerText(), a.Value()))

    // Prints a sequence of tuples
    // href |> Seq.iter (fun x -> printfn "%s %s" <|| x)


    let links = href |> Seq.map snd
                  // |> Seq.map (fun x -> snd x)

    // links |> Seq.iter (fun x -> printfn "%s" x)


    // Stop words
    let stopEs = List.ofArray <| (StopEs.GetSample()).Strings
    let stopEn = List.ofArray <| (StopEn.GetSample()).Strings

    // stopEs.Strings |> List.iter (fun x -> printf "%s " x)


    // Keywords
    let words = htmlToWords (html.ToString()) (stopEs @ stopEn)
    let topKeywords = wordCount words (wordList words)

    topKeywords
        |> List.distinct
        |> List.iter (fun x -> printf "(%s, %d) " <|| x)


    0 // return an integer exit code

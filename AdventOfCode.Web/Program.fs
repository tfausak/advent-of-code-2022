open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Hosting
open System
open System.IO
open System.Threading.Tasks

let getRoot (context: HttpContext) : Task =
    context.Response.ContentType <- "text/html;charset=utf-8"

    context.Response.WriteAsync
        """<!doctype html>
        <html lang='en-us'>
            <head>
                <meta charset='utf-8'>
                <meta name='viewport' content='initial-scale = 1, width = device-width'>
                <title>Advent of Code 2022</title>
            </head>
            <body>
                <h1>Advent of Code 2022</h1>
                <p>
                    <a href='https://github.com/tfausak/advent-of-code-2022'>github.com/tfausak/advent-of-code-2022</a>
                </p>
                <form action='/solve' method='post'>
                    <fieldset>
                        <legend>Solve</legend>
                        <ol>
                            <li>
                                <label for='day'>Day</label>
                                <input id='day' max='25' min='1' name='day' placeholder='1' required type='number'>
                            </li>
                            <li>
                                <label for='part'>Part</label>
                                <input id='part' max='2' min='1' name='part' placeholder='1' required type='number'>
                            </li>
                            <li>
                                <label for='input'>Input</label>
                                <textarea id='input' name='input' placeholder='...' required></textarea>
                            </li>
                            <li>
                                <button type='submit'>Solve</button>
                            </li>
                        </ol>
                    </fieldset>
                </form>
            </body>
        </html>
        """

let postSolve (context: HttpContext) : Task =
    task {
        let! form = context.Request.ReadFormAsync()
        let day = form[ "day" ].ToString() |> int
        let part = form[ "part" ].ToString() |> int
        let input = form[ "input" ].ToString().ReplaceLineEndings("\n")
        context.Response.ContentType <- "text/plain;charset=utf-8"

        match AdventOfCode.Library.Solver.solve day part input with
        | None ->
            context.Response.StatusCode <- 501

            ignore
            <| context.Response.WriteAsync $"I don't know how to solve day %A{day} part %A{part}."
        | Some output -> ignore <| context.Response.WriteAsync output
    }

[<EntryPoint>]
let main (arguments: string array) : int =
    let app = WebApplication.Create(arguments)
    ignore <| app.MapGet("/", Func<HttpContext, Task> getRoot)
    ignore <| app.MapPost("/solve", Func<HttpContext, Task> postSolve)
    app.Run()
    0

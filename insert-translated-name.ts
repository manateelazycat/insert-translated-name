import { DenoBridge } from "https://deno.land/x/denobridge@0.0.1/mod.ts"

const bridge = new DenoBridge(Deno.args[0], Deno.args[1], Deno.args[2], messageDispatcher)

async function messageDispatcher(message: string) {
    const [content, style, buffername, placeholder] = JSON.parse(message)[1]
    
    const cmd = ["crow", "-t", "en", "--json", content]
    const process = Deno.run({ cmd: cmd, stdout: "piped", stderr: "piped" })
    const output = await process.output()
    const translation = JSON.parse(new TextDecoder().decode(output))["translation"]

    bridge.evalInEmacs(`(insert-translated-name-update-translation-in-buffer "${content}" "${style}" "${translation}" "${buffername}" "${placeholder}")`)
}

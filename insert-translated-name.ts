import { DenoBridge } from "https://deno.land/x/denobridge@0.0.1/mod.ts"
import puppeteer from "https://deno.land/x/pptr@1.2.0/mod.ts";

const bridge = new DenoBridge(Deno.args[0], Deno.args[1], Deno.args[2], messageDispatcher)

async function messageDispatcher(message: string) {
    const args = JSON.parse(message)
    
    const string = args[0]
    const style = args[1][0]
    const buffername = args[1][1]
    const placeholder = args[1][2]
    
    bridge.messageToEmacs("Insert translation...")
    await page.goto('https://www.deepl.com/translator');
    await page.type('.lmt__source_textarea', string)
    
    bridge.messageToEmacs("Waiting translation...")
    await page.waitForFunction('document.querySelector(".lmt__target_textarea").value != ""');
    const translation = await page.$eval(".lmt__target_textarea", el => el.value.trim())
    
    bridge.messageToEmacs("Finish translate.")
    bridge.evalInEmacs(`(insert-translated-name-update-translation-in-buffer "${string}" "${style}" "${translation}" "${buffername}" "${placeholder}")`)
}

const browser = await puppeteer.launch({
    executablePath: '/usr/bin/google-chrome-stable',
    headless: true
});
const page = await browser.newPage();



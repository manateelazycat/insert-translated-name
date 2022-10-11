import { DenoBridge } from "https://deno.land/x/denobridge@0.0.1/mod.ts"
import puppeteer from "https://deno.land/x/pptr@1.2.0/mod.ts";

const bridge = new DenoBridge(Deno.args[0], Deno.args[1], Deno.args[2], messageDispatcher)

async function messageDispatcher(message: string) {
    
    const data = JSON.parse(message)
    console.log(message, data)
    
    const string = data[0]
    const style = data[1][0]
    const buffername = data[1][1]
    const placeholder = data[1][2]
    
    await page.goto('https://www.deepl.com/translator');
    await page.type('.lmt__source_textarea', string)
    await page.waitForFunction('document.querySelector(".lmt__target_textarea").value != ""');

    const translation = await page.evaluate(() => {
        return document.querySelector(".lmt__target_textarea").value.trim()
    });
    
    bridge.evalInEmacs(`(insert-translated-name-update-translation-in-buffer "${string}" "${style}" "${translation}" "${buffername}" "${placeholder}")`)
}

const browser = await puppeteer.launch({
    executablePath: '/usr/bin/google-chrome-stable',
    headless: true
});
const page = await browser.newPage();

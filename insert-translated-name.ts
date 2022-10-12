import { DenoBridge } from "https://deno.land/x/denobridge@0.0.1/mod.ts"
import puppeteer from "https://deno.land/x/pptr@1.2.0/mod.ts";

const bridge = new DenoBridge(Deno.args[0], Deno.args[1], Deno.args[2], messageDispatcher)

async function messageDispatcher(message: string) {
    const [content, style, buffername, placeholder] = JSON.parse(message)[1]

    // Don't call goto API everytime, it's slow.
    bridge.messageToEmacs("Open DeepL website...")
    if (page.url() !== "https://www.deepl.com/translator") {
        await page.goto('https://www.deepl.com/translator');
    }

    bridge.messageToEmacs("Insert translation...")
    // Clean translation textarea to wait next translation.
    await page.$eval('.lmt__target_textarea', el => el.value = '')

    // Fill content in input textarea.
    await page.$eval('.lmt__source_textarea', (el, content) => {
        el.value = content
    }, content)

    // Type Enter to trigger send translation request
    await page.type('.lmt__source_textarea', "\n")

    bridge.messageToEmacs("Waiting translation...")
    // Wait translation.
    await page.waitForFunction('document.querySelector(".lmt__target_textarea").value != ""');
    const translation = await page.$eval(".lmt__target_textarea", el => el.value.trim())

    bridge.messageToEmacs("Finish translate.")
    bridge.evalInEmacs(`(insert-translated-name-update-translation-in-buffer "${content}" "${style}" "${translation}" "${buffername}" "${placeholder}")`)
}

const browser = await puppeteer.launch({
    executablePath: '/usr/bin/google-chrome-stable',
    headless: true              // set false to launch chrome for debug
});
const page = await browser.newPage();

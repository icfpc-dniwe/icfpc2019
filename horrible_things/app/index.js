"use strict"

const fs = require('fs')
const path = require('path')
const { promisify } = require('util')
const mime = require('mime-types')
const jsdom = require("jsdom");
const _FileList = require('../node_modules/jsdom/lib/jsdom/living/generated/FileList.js')
const jsdomUtils = require('../node_modules/jsdom/lib/jsdom/living/generated/utils.js')

const { JSDOM } = jsdom;

const readFileAsync = promisify(fs.readFile);
const statAsync = promisify(fs.stat);
const setTimeoutAsync = promisify(setTimeout);

const tryOrWait = async (cb) => {
    while (true) {
        await setTimeoutAsync(0);
        const r = await cb();
        if (r) {
            return r;
        }
    }
}

const main = async () => {
    if (process.argv.length < 4)
    {
        console.log("Usage: " + __filename + " task solution");
        return;
    }
    const taskPath = process.argv[2]
    const solutionPath = process.argv[3]
    console.log("Checking task: '" + taskPath + "' solution: '" + solutionPath + "'");
    
    const URL = "https://icfpcontest2019.github.io/solution_checker/"
    const options = {
        includeNodeLocations: true,
        pretendToBeVisual: true,
        resources: "usable",
        runScripts: "dangerously"
    }

    const dom = await JSDOM.fromURL(URL, options);
    const { window } = dom
    const { document } = window

    const loadFile = async (input, filePath) => {
        const { mtimeMs: lastModified, size } = await statAsync(filePath);
        const text = await readFileAsync(filePath);
        const file = new window.File(
            [text],
            path.basename(filePath),
            {
                lastModified,
                type: mime.lookup(filePath) || '',
            }
        );
        const fileListImpl = _FileList.createImpl();
        fileListImpl.push(file);
        const fileList = jsdomUtils.wrapperForImpl(fileListImpl);
        _FileList.convert(fileList);

        Object.defineProperty(input, 'files', {
            value: fileList,
            writable: false,
        })
    }

    const result = await tryOrWait(async () => {
        const btnSubmitTask = document.getElementById("submit_task");
        const btnSubmitSolution = document.getElementById("submit_solution");
        const btnCheck = document.getElementById("execute_solution");
        const outputField = document.getElementById("output");
	      if (!(btnSubmitTask && btnSubmitSolution && btnCheck && outputField)) return;

        await loadFile(btnSubmitTask, taskPath);
        btnSubmitTask.click();

        await loadFile(btnSubmitSolution, solutionPath);
        btnSubmitSolution.click();

        btnCheck.click();

        return await tryOrWait(async () => {
            if (outputField.textContent.length === 0) return;
            return outputField.textContent;
        })
    });

    console.log(result);
}

main()

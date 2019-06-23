'use strict;'

const fs = require('fs')
const path = require('path')
const mime = require('mime-types')
const jsdom = require("jsdom");
const { addFileList } = require('addFileList')

const { JSDOM } = jsdom;

if (process.argv.length < 4)
{
    console.log("Usage: " + __filename + " task solution");
    process.exit(1);
}
var taskPath = process.argv[2]
var solutionPath = process.argv[3]
console.log("Checking task: '" + taskPath + "' solution: '" + solutionPath + "'");


var URL = "https://icfpcontest2019.github.io/solution_checker/"
var options = {
    includeNodeLocations: true,
    pretendToBeVisual: true,
    resources: "usable",
    runScripts: "dangerously"
}
JSDOM.fromURL(URL, options).then(dom => {
    setTimeout(() => {
        //console.log(dom.serialize());

        const { window } = dom
        const { document } = window

        const btnSubmitTask = document.getElementById("submit_task");
        addFileList(btnSubmitTask, taskPath);

        const btnSubmitSolution = document.getElementById("submit_solution");
        addFileList(btnSubmitSolution, solutionPath)

        const btnCheck = document.getElementById("execute_solution");
        
        btnSubmitTask.click();
        btnSubmitSolution.click();

        setTimeout(() => {
            btnCheck.click();
            setTimeout(() => {
                const outputField = document.getElementById("output");
                console.log(dom.serialize());
                console.log(outputField.textContent);
            }
            , 5000);
        }, 1000);
    }, 1000)
});
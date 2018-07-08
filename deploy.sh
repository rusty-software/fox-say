#!/bin/bash
set -e
lein clean
lein cljsbuild once min
cd resources/public
git init
git add .
git commit -m "Deploy to GitHub Pages"
git push --force --quiet "git@github.com:rusty-software/fox-say.git" master:gh-pages
rm -fr .git

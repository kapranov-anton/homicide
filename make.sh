#!/bin/bash
(cd frontend && elm-make --warn Main.elm --output main.js)
mv frontend/main.js docker/html/main.js
cp html/* docker/html/

@echo off
msbuild OpenEtw.sln /t:Build /p:Configuration=Release /p:Version=0.1.1.4

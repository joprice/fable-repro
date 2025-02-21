watch:
	dotnet clean && \
		dotnet fable watch --runWatch node Program.fs.js

build:
	dotnet clean && \
		dotnet fable clean --yes && \
		dotnet fable --test:MSBuildCracker --noCache --runScript

watch-release:
	dotnet fable watch -c Release --runWatch node Program.fs.js 

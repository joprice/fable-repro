watch:
	dotnet clean && \
		dotnet fable watch --runWatch node Program.fs.js

build:
	dotnet clean && \
		dotnet fable --runWatch node Program.fs.js

watch-release:
	dotnet fable watch -c Release --runWatch node Program.fs.js 

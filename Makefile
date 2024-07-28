watch:
	dotnet fable clean --yes && \
		dotnet fable watch fable-repro.fsproj --runWatch node Program.fs.js

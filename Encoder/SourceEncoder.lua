--------------------------------------------------
-- QuiNES source encoder
--------------------------------------------------

--------------------------------------------------
-- Setting
--------------------------------------------------

local inputFile		= "../QuiNES.asm"
local outputFile	= "SourceEncoded.asm"

local indexLabel	= "SrcIndex:"
local indexCount	= 4
local indexAlign	= 2
local codeLabel		= "SrcData:"
local codeAlign		= 32
local defineMacro	= [[  SD "%s"]]

local encodeStop	= indexLabel

local warningLength	= 32 - 4
local warningString	= "\t; WARNING :"

--------------------------------------------------
-- Utility
--------------------------------------------------

local format	= string.format

function fprintf(fp, f, ...)
	fp:write(format(f, ...))
end

--------------------------------------------------
-- Main
--------------------------------------------------

local indexList		= {}
local codeList		= {}

-- Add blank line
indexList[""]			= 0
codeList[#codeList + 1]		= ""

-- Read from file
for line in io.lines(inputFile) do
	local index	= indexList[line]
	if(index)then
		-- exist, add index
		indexList[#indexList + 1]	= index
	else
		-- not exist, add index and code
		index				= #codeList
		indexList[#indexList + 1]	= index
		indexList[line]			= index
		codeList[#codeList + 1]		= line
	end

	if(line == encodeStop)then
		break
	end
end

print(format("Information : Index count	= %d", #indexList))
print(format("Information : Code count	= %d", #codeList))

-- Write to file
local fp		= io.open(outputFile, "w")
if(not fp)then
	print(format("Error : Failed to open the output file. (%s)", outputFile))
	return
end

-- Index
fprintf(fp, "%s", indexLabel)
for i=1, #indexList, indexCount do
	fprintf(fp, "\n  .dw ")

	fprintf(fp, "$%03X", indexList[i])
	for j=2, indexCount do
		local index	= indexList[i + j - 1]	or 0
		fprintf(fp, ",$%03X", index)
	end
end
fprintf(fp, "\n")
fprintf(fp, "\n")

-- Source
fprintf(fp, "  Align %d\n", codeAlign)
fprintf(fp, "%s", codeLabel)
fprintf(fp, "\n")
for i=1, #codeList do
	local code	= codeList[i]
	code	= code:gsub("\\", "\\\\")
	code	= code:gsub("\"", "\\\"")
	code	= format(defineMacro, code)

	local warning	= ""
	local over	= #code - warningLength

	if(over > 0)then
		warning	= format("%s +%d", warning, over)
	end
	if(code:find("\t"))then
		warning	= format("%s Tab", warning)
	end
	if(#warning > 0)then
		print(format("Warning : [%4d]%s", i, warning))
		print(format("Warning :      > %s", code))

		warning	= format("%s%s", warningString, warning)
	end

	fprintf(fp, "%s%s\n", code, warning, "\n")
end

fp:close()



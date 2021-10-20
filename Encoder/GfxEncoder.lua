--------------------------------------------------
-- QuiNES GFX encoder
--------------------------------------------------

--------------------------------------------------
-- Setting
--------------------------------------------------

local inputFile		= "../Graphics/GFX_Font.bin"
local outputFile	= "GfxEncoded.asm"

local rowCount		= 8
local defineMacro	= [[  CD ]]

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

local fp

-- Read from file
fp	= io.open(inputFile, "rb")
if(not fp)then
	print(format("Error : Failed to open the input file. (%s)", inputFile))
	return
end

local inputData		= fp:read("*a")
local inputBytes	= {inputData:byte(1, #inputData)}

-- Write to file
fp	= io.open(outputFile, "w")
if(not fp)then
	print(format("Error : Failed to open the output file. (%s)", outputFile))
	return
end

fprintf(fp, "  .bank 2\n  .org $0000")
for i=1, #inputBytes do
	if(((i - 1) % rowCount) == 0)then
		fprintf(fp, "\n")
		fprintf(fp, defineMacro)
	else
		fprintf(fp, ",")
	end
	
	fprintf(fp, "%02X", inputBytes[i])
end

fprintf(fp, "\n")

fp:close()



--------------------------------------------------
-- QuiNES Test - Compare VRAM
-- usage:
--   Mesen.exe --testrunner ..\QuiNES.nes VramCompare.lua
--------------------------------------------------

--------------------------------------------------
-- Setting
--------------------------------------------------

local inputSource	= "..\\QuiNES\\QuiNES.asm"	-- Emulator base path
local outputFile	= "VramDump.asm"

local startColumn	= 2
local lineColumn	= 32

local startWaitFrame	= 5

local yPositionAddress	= 0x0012
local vramDstAddress	= 0x001C
local scrollStopDetect	= 5

--------------------------------------------------
-- Utility
--------------------------------------------------

local function ReadByte(addr)
	return emu.read(addr, emu.memType.cpu)
end
local function ReadWordLittleEndian(addr)
	local low	= emu.read(addr, emu.memType.cpu)
	local high	= emu.read(addr + 1, emu.memType.cpu)
	return (high << 8) | low
end

local function log(format, ...)
	local message	= string.format(format, ...)
	emu.log(message)
	print(message)
end

local function removeLastBlankLine(t)
	while(t[#t] == "")do
		table.remove(t, #t)
	end
end

--------------------------------------------------
-- Main
--------------------------------------------------

local vramCode			= {}
local dumpExit			= false
local pageDumped		= false
local previousLineLength	= -1
local yPosition			= -1
local scrollStopFrame		= 0

function DumpVram()
	if(pageDumped)then
		return
	end
	pageDumped	= true

	for i=0, 63 do
		if((i % 32) < 30)then
			local vramAddr	= 0x2000 + i * 32
			if(i >= 32)then
				vramAddr	= vramAddr + 0x0800
			end

			local char	= string.char
			local vramStr	= ""
			for j=startColumn, 30 do
				local c	= emu.read(vramAddr + j, emu.memType.ppu)
				if(c == 0)then
					break
				end
				vramStr	= vramStr .. char(c)
			end

			local nowLineLength	= #vramStr
			if((#vramCode > 0) and (previousLineLength == 0) and (nowLineLength == 0))then
				dumpExit	= true
				break
			end
			previousLineLength	= nowLineLength

			if((#vramCode > 0) or (nowLineLength > 0))then
				vramCode[#vramCode + 1]	= vramStr
			end
		end
	end
end
function DumpToFile()
	local fp	= io.open(outputFile, "w")
	if(fp == nil)then
		log("[Error] Failed to open the output file.")
		emu.stop(1)
	end

	for i=1, #vramCode do
		fp:write(vramCode[i])
		fp:write("\n")
	end

	fp:close()
end
function CompareWithSource()
	local srcLines		= {}
	local failed		= false

	for line in io.lines(inputSource) do
		srcLines[#srcLines + 1]	= line
	end

	removeLastBlankLine(srcLines)
	if(#srcLines == #vramCode)then
		log("[Test Pass] line count : expected = %d / actual = %d",
			#srcLines, #vramCode
		)
	else
		log("[Test Fail] line count : expected = %d / actual = %d",
			#srcLines, #vramCode
		)
		emu.stop(2)
	end

	for i=1, #srcLines do
		local expected	= srcLines[i]
		local actual	= vramCode[i]
		if(actual ~= expected)then
			log("[Test Fail] %d : expected = \"%s\" / actual = \"%s\"",
				i, expected, actual
			)
			failed	= true
		end
	end

	if(not failed)then
		log("[Test Pass] lines code matched")
	else
		emu.stop(3)
	end
end
function ExitScript()
	removeLastBlankLine(vramCode)

	log("[Info] Write to file...")
	DumpToFile()

	log("[Info] Compare with source file...")
	CompareWithSource()

	log("[Info] Test finished")
	emu.stop(0)
end

function UpdatePpuDstAddr(address, value)
	-- check VRAM dst address
	if(((value * 256) + ReadByte(vramDstAddress)) == (0x2000 + startColumn))then
		DumpVram()
		log("[Debug] Dump VRAM... (%d lines)", #vramCode)
	else
		pageDumped	= false
	end
end

log("[Info] Wait %d frames...", startWaitFrame)

local frameWaitCount		= 0
local vramCheckInstalled	= false
local nmiCallback
nmiCallback	= emu.addEventCallback(function()
	-- wait 5 frames
	frameWaitCount	= frameWaitCount + 1
	if(startWaitFrame < frameWaitCount)then
		-- add PPUADDR update callback
		if(not vramCheckInstalled)then
			emu.addMemoryCallback(UpdatePpuDstAddr, emu.memCallbackType.cpuWrite, vramDstAddress + 1)
			vramCheckInstalled	= true
		end

		-- override input
		emu.setInput(0, {down = true, up = false})

		-- detect scroll stop
		local yPos	= ReadWordLittleEndian(yPositionAddress)
		if(yPos == yPosition)then
			log("[Debug] Stop frame... (%d)", scrollStopFrame)
			scrollStopFrame	= scrollStopFrame + 1
		else
			scrollStopFrame	= 0
		end
		yPosition	= yPos
		if(scrollStopFrame > scrollStopDetect)then
			DumpVram()
			log("[Debug] Dump VRAM... (%d lines) (last)", #vramCode)
			dumpExit	= true
		end

		-- exit script
		if(dumpExit)then
			emu.removeEventCallback(nmiCallback, emu.eventType.nmi)
			ExitScript()
		end
	end
end, emu.eventType.nmi)

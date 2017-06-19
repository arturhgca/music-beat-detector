require("modules/dsp/luafft/luafft")
require("modules/util/string_operations")
require("modules/util/stats")

abs = math.abs
new = complex.new --required for using the FFT function.
UpdateSpectrum = false
beatslog = table
beat = table
log_fft = false
show_beats = not log_fft

--Song = 
--beats_file = 
--sample_counter = 

--sum_beats = 0

function devide(list, factor)
	for i,v in ipairs(list) do list[i] = list[i] * factor end
	-- This function multiplies every value in the list of frequencies for a given constant.Think of it as a sensibility setting.
end


function love.load()
	SoundData = love.sound.newSoundData(Song) --You need to load the song both to obtain it's data AND play it.
	Size = 1024 --The amount of frequencies to obtain as result of the FFT process. 
	Frequency = 44100 --The sampling rate of the song, in Hz
	length = Size / Frequency -- The size of each frequency range of the final generated FFT values.

	Music = love.audio.newSource(Song)
	Music:play()
	Window = love.window.setMode(1024, 768, {resizable=true, vsync=true})

	if(log_fft) then
		fftlog = love.filesystem.newFile(Song .. ".beats")
		fftlog:open("w")
	end
	
	if(show_beats) then
		beatslog = table
		for line in love.filesystem.lines(beats_file) do
			table.insert(beatslog, line)
		end
	end
	
	beat_timer = {}
	beat_timer[1] = love.timer.getTime()
	beat_timer[2] = beat_timer[1]
	beat_timer[3] = beat_timer[1]
	beat_timer[4] = beat_timer[1]
	
	bpm = {}
	bpm[1] = {}
	bpm[1][1] = 0
	bpm[2] = {}
	bpm[2][1] = 0
	bpm[3] = {}
	bpm[3][1] = 0
	bpm[4] = {}
	bpm[4][1] = 0
	new_beat_timer = {}
	beat_timer_delta = {}
	
	mode_bpm = {}
	mode_bpm[1] = {}
	mode_bpm[1][2] = 0
	mode_bpm[2] = {}
	mode_bpm[2][2] = 0
	mode_bpm[3] = {}
	mode_bpm[3][2] = 0
	mode_bpm[4] = {}
	mode_bpm[4][2] = 0
	
	bpm_all = {}
	mode_bpm_all = 0
	mean_bpm_all = 0
	
	flip = true
end


function love.update()
	ScreenSizeW = love.graphics.getWidth() --gets screen dimensions.
	ScreenSizeH = love.graphics.getHeight() --gets screen dimensions.


	local MusicPos = Music:tell( "samples" ) --Returns the current sample being played by the engine.
	local MusicSize = SoundData:getSampleCount() --Obtain the size of the song in samples, so you can keep track of when it's gonna end.
	-- if MusicPos >= MusicSize - 1536 then love.audio.rewind(Music) end --Rewinds the song when the music is almost over.

	local List = {} --We'll fill this with sample information.

	for i= MusicPos, MusicPos + (Size-1) do
	   CopyPos = i
	   if i + 2048 > MusicSize then i = MusicSize/2 end --Make sure you stop trying to copy stuff when the song is *almost* over, or you'll wind up getting access errors!
	   
	   List[#List+1] = new(SoundData:getSample(i*2), 0) --Copies every sample to the list, which will be fed for the FFT calculation engine.
	   -- In this case, we're fetching the Right channel samples, hence the "i*2". For the left channel, use "i*2+1". If it's a mono music, use "i*2" and you should be good.
	   -- The "new" function used above is for generating complex numbers. The FFT function only works if given a table of complex values, and it returns a table of this kind.
	end

	spectrum = fft(List, false) --runs your list through the FFT analyzer. Returns a table of complex values, all properly processed for your usage.
	--An FFT converts audio from a time space to a frequency space, so you can analyze the volume level in each one of it's frequency bands.

	devide(spectrum, 10) --Multiply all obtained FFT freq infos by 10.
	if(log_fft) then
		sample_fft = ""
		for i = 1, #spectrum/8 do
			sample_fft = sample_fft .. spectrum[i]:abs()*0.7 .. " "
		end
		love.filesystem.append(Song .. ".fft", sample_fft .. "\n")
	end

	if(show_beats and flip) then
		--sum_beats = 0
		--print(sample_counter)
		sample_counter = sample_counter + 1
		if(sample_counter>0) then
			current_beats = beatslog[sample_counter]:split(' ')
			number_of_freqs = #current_beats
			beat = {}
			for i, v in ipairs(current_beats) do
				if v == 'NA' then
					beat[i] = 0
				else
					beat[i] = 1
					new_beat_timer[i] = love.timer.getTime()
					beat_timer_delta[i] = new_beat_timer[i] - beat_timer[i]
					table.insert(bpm[i], 60000 / (beat_timer_delta[i] * 1000))
					beat_timer[i] = new_beat_timer[i]
					mode_bpm[i] = mode(bpm[i])
					table.insert(bpm_all,bpm[i][#bpm[i]])
					--print(bpm[i][2])
					--sum_beats = sum_beats + v
				end
			end
			if(#bpm_all > 1) then
				mode_bpm_all = mode(bpm_all)[2]
				--mean_bpm_all = 0
				--for j, w in ipairs(bpm_all) do
				--	mean_bpm_all = mean_bpm_all + w
				--end
				--mean_bpm_all = mean_bpm_all / #bpm_all
			end
		end
	end
	
	flip = not flip
	
	UpdateSpectrum = true --Tells the draw function it already has data to draw
end


function love.draw()

	if UpdateSpectrum then
		--for i = 1, #spectrum/8 do --In case you want to show only a part of the list, you can use #spec/(amount of bars). Setting this to 1 will render all bars processed.
		--	love.graphics.rectangle("line", i*7, ScreenSizeH, 7, -1*(spectrum[i]:abs()*0.7)) --iterate over the list, and draws a rectangle for each band value.
		--	love.graphics.print("@ "..math.floor((i)/length).."Hz "..math.floor(spectrum[i]:abs()*0.7), ScreenSizeW-90,(12*i)) --prints the frequency and it's current value on the screen.
		--	love.graphics.print(CopyPos, 0, 0) --Current position being analyzed.
		--	love.graphics.print(SoundData:getSampleCount(), 0, 20) --Current size of song in samples.
		--  
		--end

		print(mode_bpm_all)
		love.graphics.print(mode_bpm_all, 0, 0)
		if(show_beats and sample_counter > 0) then
			for i, v in ipairs(beat) do
				--sum = 0
				--for j, w in ipairs(bpm[i]) do
				--	sum = sum + w
				--end
				--sum = sum / #bpm[i]
				--love.graphics.print(sum, i*200, 100)
				love.graphics.print(mode_bpm[i][2], i*200, i*25)
				love.graphics.rectangle("fill", 100+(i-1)*(ScreenSizeW-100)/#beat, ScreenSizeH/4, 200*v, 400*v)
			end
		end
		
		--love.graphics.rectangle("fill", (ScreenSizeW-100)/2, ScreenSizeH/2, 100*sum_beats, 200*sum_beats)
	end
  
end
import subprocess,os,struct

# BTW convert wav to mp3: ffmpeg -i input.wav -codec:a libmp3lame -b:a 330k output.mp3

sox = r"k:\progs\sox-14-4-2\sox.exe"

wav_files = ["jump.wav","credit.wav",
"eat.wav","start_music.wav","enemy_falling.wav","enemy_hit.wav","ping.wav",
"enemy_killed.wav","player_killed.wav","lose_bonus.wav"]
outdir = "../sounds"


sampling_rate = 22050
alt_sampling_rate = 16000
nb_duplicates = 2


for wav_file in wav_files:
    raw_file = os.path.join(outdir,os.path.splitext(os.path.basename(wav_file))[0]+".raw")
    def get_sox_cmd(sr,output):
        return [sox,"--volume","1.0",wav_file,"--channels","1","--bits","8","-r",str(sr),"--encoding","signed-integer",output]
    used_sampling_rate = alt_sampling_rate if "loop_fright" in wav_file else sampling_rate

    cmd = get_sox_cmd(used_sampling_rate,raw_file)

    subprocess.check_call(cmd)
    with open(raw_file,"rb") as f:
        contents = f.read()
    # pre-pad with 0W, used by ptplayer for idling
    if contents[0] != b'\x00' and contents[1] != b'\x00':
        # add zeroes
        with open(raw_file,"wb") as f:
           f.write(struct.pack(">H",0))
           f.write(contents)


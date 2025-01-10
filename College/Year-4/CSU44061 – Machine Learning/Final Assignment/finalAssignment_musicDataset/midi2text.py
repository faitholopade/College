# -*- coding: utf-8 -*-
"""
@author: Giovanni Di Liberto (I cleaned it up a bit just removed the parts
that weren’t relevant to getting MIDI file)
See description in the assignment instructions.
"""
import os
from mido import MidiFile, MidiTrack, Message
# Define the note dictionary
NOTE_FREQUENCIES = {
'C': 261.63,
'c': 277.18, # C#
'D': 293.66,
'd': 311.13, # D#
'E': 329.63,
'F': 349.23,
'f': 369.99, # F#
'G': 392.00,
'g': 415.30, # G#
'A': 440.00,
'a': 466.16, # A#
'B': 493.88,
}
# Map MIDI note numbers to note names (ignoring octaves)
MIDI_NOTE_TO_NAME = {
0: 'C', 1: 'c', 2: 'D', 3: 'd', 4: 'E', 5: 'F', 6: 'f', 7: 'G', 8:
'g', 9: 'A', 10: 'a', 11: 'B'
}
# Function to convert text sequence to MIDI
def text_sequence_to_midi(sequence, output_path):
    midi = MidiFile()
    track = MidiTrack()
    midi.tracks.append(track)
# Remove spaces from the sequence
    sequence = sequence.replace(' ', '')
    for note in sequence:
        if note == 'R':
        # Add a rest (note_off with some duration)
            track.append(Message('note_off', note=0, velocity=0,  time=480))
        else:
# Map the note to a MIDI pitch
            midi_note =list(MIDI_NOTE_TO_NAME.keys())[list(MIDI_NOTE_TO_NAME.values()).index(note)]
            pitch = midi_note + 12 * 5
# Add note_on and note_off messages
    track.append(Message('note_on', note=pitch, velocity=64,time=0))
    track.append(Message('note_off', note=pitch, velocity=64,time=480))
    midi.save(output_path)
# Generated melody sequence
generated_sequence ="FCaCCdCFgFdFgFRFFCaCCFddCaCagCCdCagFFRFCaCCdCdCagFFRFFggFgFaCCdCdCaggFFRFFggFgFaCCdCdCagFFRFFggFgFaC"
# Output MIDI file path
output_path = "generated_melody.mid"
# Convert and save as MIDI
text_sequence_to_midi(generated_sequence, output_path)
print(f"Generated melody saved as {output_path}")
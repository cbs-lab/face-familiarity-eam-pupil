#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Batch Processing Script for Multiple Participants
==================================================

Processes multiple participants' data and combines into single output files.

Directory structure expected:
    behavioral_folder/
        participant1_behavioral.csv
        participant2_behavioral.csv
        ...
    h5_folder/
        participant1.h5
        participant2.h5
        ...

"""

import pandas as pd
import h5py
import numpy as np
import pickle
from pathlib import Path
from datetime import datetime
import glob
import os

# ============================================
# CONFIGURATION
# ============================================

# Input directories
BEHAVIORAL_FOLDER = "/Users/rfournier/Documents/PhD_Exp/WP3/data/WP3_data/behavioural_data"  # Folder with PsychoPy CSV files
H5_FOLDER = "/Users/rfournier/Documents/PhD_Exp/WP3/data/WP3_data/pupil_data"  # Folder with H5 files

# File naming patterns (will match participant ID)
# These patterns should capture the participant ID
# Examples:
#   "743927_Test_1_*.csv" will match "743927_Test_1_2026-02-05.csv"
#   "P*.h5" will match "P301.h5", "P302.h5", etc.
BEHAVIORAL_PATTERN = "*_Test_1_*.csv"  # Pattern for behavioral files
H5_PATTERN = "*.h5"  # Pattern for H5 files

# Output files (combined across all participants)
OUTPUT_MAIN = "all_participants_pupillometry.csv"  # PupillometryR format
OUTPUT_BASELINE = "all_participants_baseline.csv"  # Baseline summary
OUTPUT_LONG = "all_participants_long_format.csv"  # Long format with all data

# Participant ID extraction
# How to extract participant ID from filenames
# Options: 'prefix', 'number', 'custom'
ID_EXTRACTION = 'number'  # Extract just the number (e.g., "743927" from "743927_Test_1.csv")

# ============================================
# HELPER FUNCTIONS
# ============================================

def extract_participant_id(filename, method='number'):
    """
    Extract participant ID from filename.
    
    Parameters:
    -----------
    filename : str
        The filename (without path)
    method : str
        'number' - extract first number found
        'prefix' - take everything before first underscore
        'custom' - define your own extraction
    """
    basename = os.path.basename(filename)
    
    if method == 'number':
        # Extract first continuous number found
        import re
        match = re.search(r'\d+', basename)
        if match:
            return match.group()
    elif method == 'prefix':
        # Take everything before first underscore
        return basename.split('_')[0]
    elif method == 'custom':
        # Define your custom extraction here
        # Example: return basename.split('_')[0]
        pass
    
    # Fallback: use filename without extension
    return os.path.splitext(basename)[0]


def find_matching_files(behavioral_folder, h5_folder, behavioral_pattern, h5_pattern):
    """
    Find matching pairs of behavioral and H5 files.
    
    Returns:
    --------
    list of dicts: [{'id': '301', 'behavioral': 'path/to/behav.csv', 'h5': 'path/to/file.h5'}, ...]
    """
    # Find all behavioral files
    behavioral_files = glob.glob(os.path.join(behavioral_folder, behavioral_pattern))
    h5_files = glob.glob(os.path.join(h5_folder, h5_pattern))
    
    print("\n" + "="*80)
    print(" FINDING PARTICIPANT FILES")
    print("="*80)
    print(f"\nBehavioral folder: {behavioral_folder}")
    print(f"  Pattern: {behavioral_pattern}")
    print(f"  Found {len(behavioral_files)} files")
    
    print(f"\nH5 folder: {h5_folder}")
    print(f"  Pattern: {h5_pattern}")
    print(f"  Found {len(h5_files)} files")
    
    # Build participant dictionary
    participants = {}
    
    # Process behavioral files
    for bf in behavioral_files:
        pid = extract_participant_id(bf, ID_EXTRACTION)
        if pid not in participants:
            participants[pid] = {}
        participants[pid]['behavioral'] = bf
    
    # Process H5 files
    for hf in h5_files:
        pid = extract_participant_id(hf, ID_EXTRACTION)
        if pid not in participants:
            participants[pid] = {}
        participants[pid]['h5'] = hf
    
    # Create matched pairs
    matched_pairs = []
    for pid, files in participants.items():
        if 'behavioral' in files and 'h5' in files:
            matched_pairs.append({
                'id': pid,
                'behavioral': files['behavioral'],
                'h5': files['h5']
            })
            print(f"\n✓ Matched participant {pid}:")
            print(f"    Behavioral: {os.path.basename(files['behavioral'])}")
            print(f"    H5: {os.path.basename(files['h5'])}")
        else:
            print(f"\n⚠️  Incomplete data for participant {pid}:")
            if 'behavioral' in files:
                print(f"    Behavioral: {os.path.basename(files['behavioral'])}")
            else:
                print(f"    Behavioral: MISSING")
            if 'h5' in files:
                print(f"    H5: {os.path.basename(files['h5'])}")
            else:
                print(f"    H5: MISSING")
    
    print(f"\n{'='*80}")
    print(f"Total matched participants: {len(matched_pairs)}")
    print(f"{'='*80}\n")
    
    return matched_pairs


# ============================================
# IMPORT PROCESSING FUNCTIONS
# ============================================

# Import all the processing functions from the single-participant script
# (These are the same functions from ExtractPupillometry_Recognition_UPDATED.py)

def reconstruct_pandas_table(h5_file, group_name):
    """Reconstruct pandas DataFrame from HDFStore format"""
    try:
        group = h5_file[group_name]
        block_info = []
        block_num = 0
        max_rows = 0
        
        while True:
            items_key = f'block{block_num}_items'
            values_key = f'block{block_num}_values'
            
            if items_key not in group or values_key not in group:
                break
            
            col_names = group[items_key][:]
            if col_names.dtype.kind in ('S', 'O'):
                col_names = [
                    name.decode('utf-8') if isinstance(name, bytes) else str(name)
                    for name in col_names
                ]
            
            values = group[values_key][:]
            n_col_names = len(col_names)
            
            if len(values.shape) == 1:
                n_rows = len(values)
                values_array = values.reshape(-1, 1)
            elif len(values.shape) == 2:
                if n_col_names == values.shape[1]:
                    values_array = values
                    n_rows = values.shape[0]
                elif n_col_names == values.shape[0]:
                    values_array = values.T
                    n_rows = values.shape[1]
                else:
                    values_array = values.T
                    n_rows = values.shape[1]
            else:
                block_num += 1
                continue
            
            block_info.append({
                'col_names': col_names,
                'values': values_array,
                'n_rows': n_rows
            })
            
            max_rows = max(max_rows, n_rows)
            block_num += 1
        
        if not block_info:
            return None
        
        all_data = {}
        for block in block_info:
            values = block['values']
            col_names = block['col_names']
            
            for i, col_name in enumerate(col_names):
                if i >= values.shape[1]:
                    all_data[col_name] = np.full(max_rows, np.nan)
                    continue
                
                col_data = values[:, i]
                if len(col_data) < max_rows:
                    padding = np.full(max_rows - len(col_data), np.nan)
                    col_data = np.concatenate([col_data, padding])
                
                all_data[col_name] = col_data
        
        if not all_data:
            return None
        
        df = pd.DataFrame(all_data)
        
        for col in df.columns:
            if df[col].dtype == object:
                try:
                    def clean_value(x):
                        if pd.isna(x):
                            return x
                        if isinstance(x, np.ndarray):
                            if len(x) > 0:
                                val = x[0]
                                return val.decode('utf-8') if isinstance(val, bytes) else str(val)
                            return ''
                        elif isinstance(x, bytes):
                            return x.decode('utf-8')
                        else:
                            return x
                    df[col] = df[col].apply(clean_value)
                except:
                    pass
        
        return df
    except Exception as e:
        print(f"Error extracting {group_name}: {e}")
        return None


def extract_messages(h5_file):
    """Extract messages from HDF5 file"""
    try:
        if 'msg' not in h5_file:
            return None
        
        msg_group = h5_file['msg']
        timestamp_values = None
        message_values = None
        
        for block_num in range(10):
            items_key = f'block{block_num}_items'
            values_key = f'block{block_num}_values'
            
            if items_key not in msg_group or values_key not in msg_group:
                continue
            
            items = msg_group[items_key][:]
            values = msg_group[values_key][:]
            
            col_names = []
            for item in items:
                if isinstance(item, bytes):
                    col_names.append(item.decode('utf-8'))
                else:
                    col_names.append(str(item))
            
            if any('time' in col.lower() for col in col_names):
                timestamp_values = values
            
            if values.dtype == object or 'msg' in str(col_names).lower():
                message_values = values
        
        if timestamp_values is not None:
            if len(timestamp_values.shape) == 2:
                timestamps = timestamp_values[:, 0]
            else:
                timestamps = timestamp_values
        else:
            return None
        
        messages = []
        if message_values is not None:
            if message_values.dtype == object:
                try:
                    if len(message_values.shape) == 2:
                        msg_data = message_values[0, 0]
                    else:
                        msg_data = message_values[0]
                    
                    unpickled = pickle.loads(bytes(msg_data))
                    
                    if isinstance(unpickled, np.ndarray):
                        messages = unpickled.flatten().tolist()
                    elif isinstance(unpickled, list):
                        messages = unpickled
                    else:
                        messages = [unpickled]
                except:
                    if len(message_values.shape) == 2:
                        for row in message_values:
                            messages.append(str(row[0]))
                    else:
                        for val in message_values:
                            messages.append(str(val))
            else:
                if len(message_values.shape) == 2:
                    messages = [str(val[0]) for val in message_values]
                else:
                    messages = [str(val) for val in message_values]
        else:
            return None
        
        min_len = min(len(timestamps), len(messages))
        timestamps = timestamps[:min_len]
        messages = messages[:min_len]
        
        df = pd.DataFrame({
            'system_time_stamp': timestamps,
            'msg': messages
        })
        
        return df
    except Exception as e:
        print(f"Error extracting messages: {e}")
        return None


def load_behavioral_data(csv_file):
    """Load behavioral data from PsychoPy CSV."""
    try:
        behav_df = pd.read_csv(csv_file, encoding='utf-8-sig')
        rec_trials = behav_df[behav_df['trial_type'] == 'recognition'].copy()
        
        if len(rec_trials) == 0:
            return None
        
        behav_data = pd.DataFrame({
            'trial_num': range(len(rec_trials)),
            'accuracy': rec_trials['key_resp_5.corr'].values,
            'rt': rec_trials['key_resp_5.rt'].values,
            'block': rec_trials['block'].values
        })
        
        return behav_data
    except Exception as e:
        print(f"Error loading behavioral data: {e}")
        return None


# Continue in next message due to length...
# PART 2: Remaining processing functions and main batch loop

# (Add this to the end of Batch_Process_Participants.py)

def parse_recognition_trials(msgs_df):
    """Parse trials from recognition task messages"""
    
    SCRAMBLED_ONSET_LEARNED = "onset_scrambled_learned"
    SCRAMBLED_ONSET_UNFAMILIAR = "onset_scrambled_unfamiliar"
    SCRAMBLED_OFFSET_LEARNED = "offset_scrambled_learned"
    SCRAMBLED_OFFSET_UNFAMILIAR = "offset_scrambled_unfamiliar"
    FACE_ONSET_FAMILIAR = "onset_familiar"
    FACE_ONSET_UNFAMILIAR = "onset_unfamiliar"
    FACE_OFFSET_FAMILIAR = "offset_familiar"
    FACE_OFFSET_UNFAMILIAR = "offset_unfamiliar"
    
    trials = []
    
    scrambled_onsets = msgs_df[
        msgs_df['msg'].isin([SCRAMBLED_ONSET_LEARNED, SCRAMBLED_ONSET_UNFAMILIAR])
    ].copy()
    
    for idx, scrambled_row in scrambled_onsets.iterrows():
        trial_data = {}
        
        scrambled_msg = scrambled_row['msg']
        if scrambled_msg == SCRAMBLED_ONSET_LEARNED:
            trial_data['scrambled_type'] = 'learned'
        else:
            trial_data['scrambled_type'] = 'unfamiliar'
        
        trial_data['scrambled_onset_time'] = scrambled_row['system_time_stamp']
        
        offset_msg = f"offset_{scrambled_msg.replace('onset_', '')}"
        scrambled_offsets = msgs_df[
            (msgs_df['msg'] == offset_msg) & 
            (msgs_df['system_time_stamp'] > trial_data['scrambled_onset_time'])
        ]
        
        if len(scrambled_offsets) == 0:
            continue
        
        trial_data['scrambled_offset_time'] = scrambled_offsets.iloc[0]['system_time_stamp']
        
        face_onsets = msgs_df[
            msgs_df['msg'].isin([FACE_ONSET_FAMILIAR, FACE_ONSET_UNFAMILIAR]) &
            (msgs_df['system_time_stamp'] > trial_data['scrambled_offset_time']) &
            (msgs_df['system_time_stamp'] < trial_data['scrambled_offset_time'] + 5000000)
        ]
        
        if len(face_onsets) == 0:
            continue
        
        face_msg = face_onsets.iloc[0]['msg']
        trial_data['face_onset_time'] = face_onsets.iloc[0]['system_time_stamp']
        trial_data['face_condition'] = 'familiar' if face_msg == FACE_ONSET_FAMILIAR else 'unfamiliar'
        
        if trial_data['face_condition'] == 'familiar':
            expected_offset = FACE_OFFSET_FAMILIAR
        else:
            expected_offset = FACE_OFFSET_UNFAMILIAR
        
        face_offsets = msgs_df[
            (msgs_df['msg'] == expected_offset) &
            (msgs_df['system_time_stamp'] > trial_data['face_onset_time'])
        ]
        
        if len(face_offsets) == 0:
            continue
        
        trial_data['trial_offset_time'] = face_offsets.iloc[0]['system_time_stamp']
        trial_data['trial_num'] = len(trials)
        
        trials.append(trial_data)
    
    return trials


def format_long_format_batch(gaze_df, trials, msgs_df, subject_id, behav_data=None):
    """Format data in long format for batch processing"""
    
    # Find columns
    left_pupil_col = None
    right_pupil_col = None
    left_x_col = None
    left_y_col = None
    right_x_col = None
    right_y_col = None
    
    for col in gaze_df.columns:
        col_lower = col.lower()
        if 'left' in col_lower:
            if 'pupil' in col_lower and 'valid' not in col_lower:
                left_pupil_col = col
            elif 'gaze_point' in col_lower or 'gaze_pos' in col_lower:
                if 'x' in col_lower or col_lower.endswith('_0'):
                    left_x_col = col
                elif 'y' in col_lower or col_lower.endswith('_1'):
                    left_y_col = col
        elif 'right' in col_lower:
            if 'pupil' in col_lower and 'valid' not in col_lower:
                right_pupil_col = col
            elif 'gaze_point' in col_lower or 'gaze_pos' in col_lower:
                if 'x' in col_lower or col_lower.endswith('_0'):
                    right_x_col = col
                elif 'y' in col_lower or col_lower.endswith('_1'):
                    right_y_col = col
    
    all_trial_data = []
    
    for trial in trials:
        trial_num = trial['trial_num']
        scrambled_type = trial['scrambled_type']
        
        trial_start = trial['scrambled_onset_time']
        trial_end = trial['trial_offset_time']
        
        trial_mask = ((gaze_df['system_time_stamp'] >= trial_start) & 
                     (gaze_df['system_time_stamp'] <= trial_end))
        trial_gaze = gaze_df[trial_mask].copy()
        
        if len(trial_gaze) == 0:
            continue
        
        trial_gaze['time'] = ((trial_gaze['system_time_stamp'] - trial_start) / 1000.0).round().astype(int)
        trial_gaze['subject'] = subject_id
        trial_gaze['trial'] = trial_num + 1
        
        trial_gaze['pupil_left'] = trial_gaze[left_pupil_col] if left_pupil_col else np.nan
        trial_gaze['pupil_right'] = trial_gaze[right_pupil_col] if right_pupil_col else np.nan
        trial_gaze['x_pos_left'] = trial_gaze[left_x_col] if left_x_col else np.nan
        trial_gaze['y_pos_left'] = trial_gaze[left_y_col] if left_y_col else np.nan
        trial_gaze['x_pos_right'] = trial_gaze[right_x_col] if right_x_col else np.nan
        trial_gaze['y_pos_right'] = trial_gaze[right_y_col] if right_y_col else np.nan
        
        trial_gaze['blink'] = 0
        both_missing = (trial_gaze['pupil_left'].isna() | (trial_gaze['pupil_left'] <= 0)) & \
                       (trial_gaze['pupil_right'].isna() | (trial_gaze['pupil_right'] <= 0))
        trial_gaze.loc[both_missing, 'blink'] = 1
        
        trial_gaze['condition'] = scrambled_type
        trial_gaze['message'] = 'NA'
        
        trial_msgs = msgs_df[
            (msgs_df['system_time_stamp'] >= trial_start) & 
            (msgs_df['system_time_stamp'] <= trial_end)
        ].copy()
        
        for _, msg_row in trial_msgs.iterrows():
            msg_time = msg_row['system_time_stamp']
            msg_text = 'fixation_baseline' if 'scrambled' in msg_row['msg'] else ('target' if 'familiar' in msg_row['msg'] or 'unfamiliar' in msg_row['msg'] else msg_row['msg'])
            
            time_diff = np.abs(trial_gaze['system_time_stamp'] - msg_time)
            closest_idx = time_diff.idxmin()
            trial_gaze.loc[closest_idx, 'message'] = msg_text
        
        if behav_data is not None and trial_num < len(behav_data):
            trial_gaze['accuracy'] = behav_data.loc[trial_num, 'accuracy']
            trial_gaze['rt'] = behav_data.loc[trial_num, 'rt']
            trial_gaze['block'] = behav_data.loc[trial_num, 'block']
        else:
            trial_gaze['accuracy'] = np.nan
            trial_gaze['rt'] = np.nan
            trial_gaze['block'] = np.nan
        
        output_cols = [
            'subject', 'trial', 'time',
            'pupil_left', 'x_pos_left', 'y_pos_left',
            'blink', 'message', 'accuracy', 'rt', 'block',
            'condition',
            'pupil_right', 'x_pos_right', 'y_pos_right'
        ]
        
        trial_data_formatted = trial_gaze[output_cols].copy()
        all_trial_data.append(trial_data_formatted)
    
    if not all_trial_data:
        return None
    
    long_format_data = pd.concat(all_trial_data, ignore_index=True)
    long_format_data.loc[long_format_data['pupil_left'] <= 0, 'pupil_left'] = np.nan
    long_format_data.loc[long_format_data['pupil_right'] <= 0, 'pupil_right'] = np.nan
    
    return long_format_data


def process_participant(participant_info):
    """Process a single participant's data"""
    
    subject_id = participant_info['id']
    h5_file_path = participant_info['h5']
    behavioral_file_path = participant_info['behavioral']
    
    print("\n" + "="*80)
    print(f" PROCESSING PARTICIPANT {subject_id}")
    print("="*80)
    
    try:
        with h5py.File(h5_file_path, 'r') as f:
            gaze_df = reconstruct_pandas_table(f, 'gaze')
            if gaze_df is None:
                print(f"❌ Failed to extract gaze data")
                return None
            
            print(f"✓ Extracted {len(gaze_df):,} gaze samples")
            
            msgs_df = extract_messages(f)
            if msgs_df is None:
                print(f"❌ Failed to extract messages")
                return None
            
            print(f"✓ Extracted {len(msgs_df)} messages")
        
        behav_data = load_behavioral_data(behavioral_file_path)
        if behav_data is not None:
            print(f"✓ Loaded behavioral data ({len(behav_data)} trials)")
        
        trials = parse_recognition_trials(msgs_df)
        if not trials:
            print(f"❌ No trials could be parsed")
            return None
        
        print(f"✓ Parsed {len(trials)} trials")
        
        long_format_data = format_long_format_batch(gaze_df, trials, msgs_df, subject_id, behav_data)
        
        if long_format_data is None:
            print(f"❌ Failed to format data")
            return None
        
        print(f"✓ Created long format data: {len(long_format_data):,} samples")
        
        return long_format_data
        
    except Exception as e:
        print(f"❌ ERROR processing participant {subject_id}: {e}")
        import traceback
        traceback.print_exc()
        return None


# ============================================
# MAIN BATCH PROCESSING
# ============================================

def main():
    """Main batch processing function"""
    
    print("\n" + "="*80)
    print(" BATCH PUPILLOMETRY DATA EXTRACTION")
    print("="*80)
    print(f"\nBehavioral folder: {BEHAVIORAL_FOLDER}")
    print(f"H5 folder: {H5_FOLDER}")
    print(f"\nOutput files:")
    print(f"  • {OUTPUT_LONG}")
    print("="*80)
    
    # Find matching participant files
    participants = find_matching_files(
        BEHAVIORAL_FOLDER, 
        H5_FOLDER, 
        BEHAVIORAL_PATTERN, 
        H5_PATTERN
    )
    
    if not participants:
        print("\n❌ No matching participant files found!")
        print("\nCheck:")
        print("  1. Folder paths are correct")
        print("  2. File patterns match your naming convention")
        print("  3. Files exist in both folders")
        return
    
    # Process each participant
    all_long_data = []
    successful = 0
    failed = 0
    
    for p_info in participants:
        result = process_participant(p_info)
        if result is not None:
            all_long_data.append(result)
            successful += 1
        else:
            failed += 1
    
    # Combine all participants
    if all_long_data:
        print("\n" + "="*80)
        print(" COMBINING ALL PARTICIPANTS")
        print("="*80)
        
        combined_long = pd.concat(all_long_data, ignore_index=True)
        
        print(f"\nCombined dataset:")
        print(f"  Total samples: {len(combined_long):,}")
        print(f"  Participants: {combined_long['subject'].nunique()}")
        print(f"  Trials: {combined_long.groupby('subject')['trial'].max().sum()}")
        
        # Save combined file
        combined_long.to_csv(OUTPUT_LONG, index=False)
        print(f"\n💾 Saved: {OUTPUT_LONG}")
        
        # Show sample
        print("\n" + "="*80)
        print(" SAMPLE DATA (first 10 rows)")
        print("="*80)
        print(combined_long.head(10).to_string(index=False))
        
        # Summary statistics
        print("\n" + "="*80)
        print(" SUMMARY BY PARTICIPANT")
        print("="*80)
        summary = combined_long.groupby('subject').agg({
            'trial': 'max',
            'pupil_left': lambda x: (~x.isna()).sum(),
            'accuracy': 'mean',
            'rt': 'mean'
        })
        summary.columns = ['Trials', 'Valid Samples', 'Mean Accuracy', 'Mean RT']
        print(summary.to_string())
        
        print("\n" + "="*80)
        print("✅ BATCH PROCESSING COMPLETE!")
        print("="*80)
        print(f"\nSuccessful: {successful} participants")
        print(f"Failed: {failed} participants")
        print(f"\nOutput file: {OUTPUT_LONG}")
        print()
        
    else:
        print("\n❌ No data could be processed!")
        print(f"\nFailed: {failed} participants")


if __name__ == '__main__':
    main()

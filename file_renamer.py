# Renames all tests in a set for shorter alphabet description.
# Run in shell with appropriate venv, suplly the path as argument in the shell.

import sys
import math
from pathlib import Path
import shutil

def rename_all(top_dir):
	""" Renames all files with longer alphabet description ("7ang_4len") to the short one ("28alph")
	
	:return: None
	"""
	count = 0
	for child in top_dir.iterdir():
		if child.is_file():
			count += cpy_rename_if_needed(child)
	print(count, "files have been renamed to include shorter alphabet description.")

def cpy_rename_if_needed(fpath: Path):
	""" Copies a renames file if a two-dimensional alphabet size is included in the name
	
	:return: 1 if renamed, 0 if not
	"""
	file_name = fpath.name
	cf = get_conf_or_none(file_name)
	if cf != -1:
		new_name = get_new_name(file_name, cf)
		ppath = fpath.parent
		new_path = ppath.joinpath(new_name)
		shutil.copy(fpath, new_path)
		return 1
	return 0

def get_new_name(prev_name, alph_size) -> str:
	""" Returns the new name of a file
	
	:param prev_name: The previous name
	:param alph_size: The size of the alphbabet used to create this file
	:return: New name
	"""
	first_index = get_index_of_first_num(prev_name, "ang")[1]
	len_tuple = get_index_of_first_num(prev_name, "len")
	last_index = len_tuple[1]
	# last index is the length of needle "len" and the number of digits before it
	last_index += len("len") + int(math.log10(len_tuple[0])) + 1
	previous_alph = prev_name[first_index: last_index]
	new_alph = str(alph_size) + "alph"
	new_name = prev_name.replace(previous_alph, new_alph)
	return new_name

def get_conf_or_none(name: str) -> int:
	""" Returns the alphabet size (multiplied length and angle count)
	If one-dimensional or other file -1
	
	:param name: The name to extract the configuration
	:return: alphabet size or -1
	"""
	if "ang_" in name and "len" in name:
		len = get_index_of_first_num(name, "len")[0]
		ang = get_index_of_first_num(name, "ang")[0]
		return ang * len
	else:
		return -1

def get_index_of_first_num(whole_str, str_after) -> tuple:
	""" Returns first index and num that has been found directly before the keyword
	
	:param whole_str: The haystack
	:param str_after: The needle after the desired num
	:return: (num, index)
	"""
	index = whole_str.index(str_after)
	index -= 1
	num = 0
	multipl = 1
	while whole_str[index].isdigit():
		num += int(whole_str[index])*multipl
		multipl *= 10
		index -= 1
	return (num, index+1)

def main():
	if len(sys.argv) <= 1:
		print("You have to specify the path where all the files to rename lie in.")
		print("Path is supplied after to run command like   > python .\file_renamer.py 'C:\CS1_R-Intro\folder'")
	else:
		ppath = Path(sys.argv[1])
		print("Trying to rename every file in", ppath)
		rename_all(ppath)
	

if __name__ == "__main__":
	main()
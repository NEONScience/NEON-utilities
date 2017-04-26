#!/usr/bin/python2.7

# ******* IMPORTANT ********: Due to file names, this script should currently only be used with litterfall files.
# This code is designed to be run from the command line: python neon-data-stacker.py <zip file path>
# It unzips a single NEON dowload file in a new target directory, next to the zip file.
# Duplicate files from different sub-zip folders will be ignored


import argparse
import os.path
from zipfile import ZipFile as zf
import re
from shutil import copyfile

# print wrapper to mind the silent argument
def print_option(message):
	if not args.silent:
		print message

# unzip a single NEON zip file
def unzip_file(file_path):
	success = False
	target_directory = None
	try:
		if os.path.isfile(file_path):

			# make a target folder to extract to
			# if the taret folder exists, add a constant string to it until it doesn't exist, then make it
			target_directory = os.path.join(os.path.dirname(file_path), os.path.basename(file_path).rstrip(".zip"))
			while True:
				if (os.path.isdir(target_directory)):	
					target_directory = target_directory + "_unzip"
				else:
					break;
			print_option("Making directory " + target_directory)
			os.makedirs(target_directory)
			
			#unzip the top level zip file
			print_option("Unzipping the top level zip file " + file_path + " to target directory " + target_directory)
			with zf(file_path) as top_level_zip:
				top_level_zip.extractall(target_directory)

			#for each member in the top level zip
			for member_file in os.listdir(target_directory):
				member_path = os.path.join(target_directory, member_file)
				if (member_file.endswith(".zip")):
					print_option("Found member zip file " + member_file + ".  Unzipping to the target directory.")
					with zf(member_path) as member:
						for submember_file_name in member.namelist():
							#write each file in the member zip to the target directory
							#but ignore duplicates
							submember_file_path = os.path.join(target_directory, submember_file_name)
							# http://stackoverflow.com/questions/17729703/extract-a-specific-file-from-a-zip-archive-without-maintaining-directory-structu
							if not os.path.isfile(submember_file_path):
								with open(submember_file_path, "wb") as f:
									f.write(member.read(submember_file_name))
								print_option("Found file: " + submember_file_name)
							else:
								print_option("Duplicate file, not included: " + submember_file_name)
					print_option("Deleting member zip file " + member_path + " after extraction.")
					os.remove(member_path)

			success = True
		else:
			print_option("The file \'" + file_path + "\' is not a valid NEON zip file.")
	except Exception as ex:
		print_option("Unable to extract the file " + file_path + ".")
		# http://stackoverflow.com/questions/9823936/python-how-do-i-know-what-type-of-exception-occurred
		template = "An exception of type {0} occurred. Arguments:\n{1!r}"
		message = template.format(type(ex).__name__, ex.args)
		print_option(message)

	return success, target_directory


parser = argparse.ArgumentParser(description="Unzip NEON data files")
parser.add_argument("zip_path", help="The path to the file to unzip")
parser.add_argument("-s", "--silent", help="Run silently", action="store_true")
args = parser.parse_args()

success, folder_path = unzip_file(args.zip_path)

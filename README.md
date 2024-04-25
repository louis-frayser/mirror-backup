# app-backup/mirror-backup

This is a backup script generator for backing up my system to external USB drives.

## Introduction
### The main task
1. Allow for declarative description of the backup tasks
   by describing which source directories are to be backed up
2. Check for common errors.
3. Generate a shell script to to the job.

### Other Tasks are
1. Check for duplicate backups actions.
2. Verifying that the source and destination paths exists and are mount points
   when that's necessary.
3. Outputting a very user-readable shell script that can be easily checked before
   running backup.
 
### Running
1. Edit the configuration (currently the *backup plan* records inside *app-data.rkt*
2. Run the program 
>  racket *mirror-backup.rkt*

    2.1 That creates the program: *mirror-backup.sh*
3. Examining the shell script.
4. Run the script:
>   **sh** *mirror-backup.sh*

5. There is a Makefile in the sources directory.  Running 
>  **make test** 
will generate the shell script and run it.

### Caveats
1. Running this script requires superuser privileges.  At least that is the 
   intention.  It cat work for anyone who has permission to write in the 
   target paths.
2. Some knowledge of Scheme could be helpful.  It is easy for a non Schemer
   to hose things up.
3. Requires Racket pagkage "Rebellion": 
> raco install -i --auto rebellion
   
### Dreams
Eventually, the configuration my be separated from the source code and
    converted into a YAML config file.  From there, even a GUI is possible
	for the purposes of configuring and ruining the backup.
	

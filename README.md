# InCollege

A job search and networking application designed specifically for college students. Built in COBOL, this terminal application simulates provides a similar experience to LinkedIn, where users can create accounts, design profiles, look up other users, and send connection requests.

## Quick Start (Students)

**Prerequisites**
- Install **Docker Desktop**: https://www.docker.com/products/docker-desktop/
  - Windows: enable **WSL 2** backend during installation.
- Install **VS Code**: https://code.visualstudio.com/
- In VS Code, install the **Dev Containers** extension (ID: `ms-vscode-remote.remote-containers`).

**Run the template**
1. Clone this repo and open it in VS Code.
2. When prompted, click **“Reopen in Container”**. (Or run *Dev Containers: Reopen in Container* from the Command Palette.)
3. Prepare the input file [InCollege-input.txt](InCollege-input.txt) to simulate all user inputs.
4. Use the provided [Makefile](Makefile) to compile the program. The executable will be created in the `bin/` directory.
5. Run in the VS Code terminal using `./bin/InCollegeDriver`.

## Features
- Docker image with **Ubuntu 22.04 + GNU COBOL (gnucobol)**.
- VS Code tasks to **build and run the active COBOL file**.
- Default UTF-8 locale configured.
- No extra installs on host OS beyond Docker + VS Code.
- Modular COBOL code with separate files for account management, profile management, and connection management.
- Persistent account, profile, and connection data storage.
- Simple I/O.
  - All input is read from a predefined input file.
  - All output is displayed on screen and identically written to an output file for record-keeping.
- Password hashing and validation.
- Profile creation with required and optional fields, including multiple experience and education entries.
  - Accessible after login through Create/Edit My Profile.
  - Fields captured include:
    - Required: First Name, Last Name, University/College, Major, Graduation Year.
    - Optional: About Me, up to 3 Experience entries, up to 3 Education entries.
  - Experience entries include Title, Company/Organization, Dates, and optional Description.
  - Education entries include Degree, University/College, and Years Attended.
  - All profile information is saved and linked to the user’s account.
  - Data persists across application restarts via sequential files or extended storage linked by username.
  - Input validation ensures required fields are correct, e.g., numeric 4-digit graduation year.
  - Modular design with dedicated sections for profile management.
  - Uses OCCURS clauses for multiple entries, PIC clauses for data types.
  - Integration with existing login from Week 1.
- Profile viewing using "View My Profile" in the main menu.
- User search functionality to find and view other users' profiles.
  - Accessible after login through Find someone you know.
  - Allows users to search for other registered InCollege users via theifull name (e.g. "John Doe")
  - The system searches for an exact match to the name given by the user.
  - If a match is found, the system displays the user's full profile in thsame manner as one's own profile.
  - If a match is not found, the system displays a message informing thuser.
- Connection request system with pending request management.
  - Available as an option after viewing another user's profile.
  - Users can send a connection request to another user, which is saved as a pending request.
  - Users can view their pending connection requests.
  - Automatic connection handling to prevent duplicate requests and self-connections.

## Primary Modules
- **InCollegeDriver.cob**: Main driver program that handles user input, displays menus, and calls the appropriate logic modules based on user actions.
- **AccountLogic.cob**: Handles account creation, login, and password management.
  - **"LA"**: Load Accounts- Counts the number of currently created accounts. Returns "Y" if less than the maximum allowed accounts (5), otherwise "N".
  - **"CU"**: Check Username- Checks whether a username is already associated with an account or not. Returns "Y" if the username was found, otherwise "N".
  - **"VP"**: Validate Password- Validates a provided password against provided rules. Returns "Y" if the password is valid, otherwise "N".
  - **"AA"**: Add Account- Adds a new account and saves it to the accounts file. Returns nothing.
  - **"AL"**: Attempt Login- Attempts to login to a user account. Returns "Y" if login was successful, otherwise "N".
- **ProfileLogic.cob**: Manages profile creation, editing, viewing, and searching.
  - **"SP"**: Save Profile- Saves the user's profile information to a profile file. Returns "Y" if the save was successful, otherwise "N".
  - **"GCP"**: Get Current Profile- Searches for the current user's profile. Returns "Y" if the profile was found, otherwise "N".
  - **"GFP"**: Get Full Profile- Searches for another user's profile by their first/last name. Returns "Y" if the profile was found, otherwise "N".
- **ConnectionLogic.cob**: Manages connection requests, including sending requests, viewing pending requests, and accepting/declining requests.
  - **"APR"**: Add Pending Request- Adds a new pending request. Returns "Y" and message "Connection request sent successfully." if request was successfully added, otherwise "N", meaning request between 2 users already exists.
  - **"GAP"**: Get All Pending Requests- Retrieves all pending requests for the current user. Returns "Y" if there are pending requests, otherwise "N".
  - **"GAC"**: Get All Connections- Retrieves all accepted connections for the current user. Returns "Y" if there are accepted connections, otherwise "N".

## Common Commands (inside the container)
```bash
# Use Makefile to compile and run
make
./bin/InCollege
```

## Main Achievements
- Created function in ConnectionLogic.cob for getting all pending connection requests for a user, and integrated it into the driver.
- Improved parameter passing for all logic modules by creating structured parameters in the driver and using those in the logic modules, reducing the number of parameters passed and improving readability.
- Improved project organization by creating build and data directories to separate compiled files and data files from source code.

## Next Steps
- Thoroughly test the new connection request feature, and fix any bugs found.
- Add epic features for this week
- Further test to find any additional bugs, and fix with previously found bugs
- Test using return messages like with ConnectionLogic.cob to try and reduce prints in driver instead of "Y/N" codes
- Continue work on implementing connection acceptance logic
- Add job logic for epics 6 and 7
- Officially get rid of InCollegeDriver.cob, since InCollege.cob is now the main driver program

## Bug List
- Passwords with multiple capital letters are not considered valid (e.g. "Cam!123Pr" is not considered valid even though it meets all requirements)
- Education fields are not being properly saved to the record when creating/editing profile

## Troubleshooting
- If VS Code doesn’t prompt to reopen in a container, run **Dev Containers: Reopen in Container** manually.
- On Windows, make sure Docker Desktop is running with **WSL 2** enabled.
- On Apple Silicon (M1/M2/M3), Docker will pull the correct multi-arch Ubuntu image automatically.
- If you see permission issues on `bin/` after pulling from a different OS, run: `sudo chmod -R a+rw bin` (inside the container).

## Folder Structure
```
.devcontainer/         # Dev container config (Dockerfile, devcontainer.json)
.vscode/               # VS Code tasks
bin/                   # Compiled executable (InCollege)
build/                 # Compiled object files
- AccountLogic.o
- ConnectionLogic.o
- ProfileLogic.o
data/                  # Persistent data files
- Accounts.dat         # Persistent storage for account information
- Connections.dat      # Persistent storage for accepted connections
- PendingRequests.dat  # Persistent storage for pending connection requests
- Profiles.dat        # Persistent storage for profile information
src/                   # COBOL source files
- AccountLogic.cob     # Logic for account management (creation, login, password validation)
- ConnectionLogic.cob  # Logic for connection management (sending requests, viewing pending requests)
- InCollege.cob        # Main driver program that handles user input and calls logic modules
- ProfileLogic.cob     # Logic for profile management (creating/editing profiles, viewing/searching profiles)
.gitignore             # Ignore compiled files and other non-source files
InCollege-input.txt    # Sample input file to simulate user input
InCollege-output.txt   # Output file where program writes output
Makefile               # Makefile to compile COBOL source files
Profilestest.dat       # Temp data for profile management (will be removed in final version)
README.md
Test-input.txt         # Additional input file for testing purposes (will be removed in final version)
```

## License
MIT

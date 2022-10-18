import pandas as pd
import requests
from bs4 import BeautifulSoup
import os
import re
from numpy import unique
from collections import Counter
import fiscalyear
fiscalyear.START_MONTH = 10

# Pull stats for all players of given type ("skaters" or "goalies")
# for all given seasons (e.g., 2022)
def pullPlayerStats(seasons,playerType):
    
    # If single season is used as input, convert to list of length 1
    if type(seasons) is not list:
        seasons = [seasons]
    
    # Iterate through all seasons
    for season in seasons:
        
        # Read in player stats for the season
        url = "https://www.hockey-reference.com/leagues/NHL_"+str(season)+"_"+playerType+".html"
        
        # Use beatifulsoup to extract unique player IDs
        page = requests.get(url)
        soup = BeautifulSoup(page.text, 'lxml')
        table1 = soup.find('table', id='stats')
        
        playerIDs = []
        for i in table1.find_all('td'):
            if i.has_attr('data-append-csv'):
                playerIDs.append(i.get('data-append-csv'))
                
        # Iterate through all players that played this season
        for player in playerIDs:
            print("Getting",player, "stats for",season)
            # Create folders to store player data if they dont exist
            basedirectory = "Data/Players/" + player
            os.makedirs(basedirectory, exist_ok=True)
            
            
            # Get player game log for current season
            url = "https://www.hockey-reference.com/players/"+player[0]+"/"+player+"/gamelog/"+str(season)
            playerstats = pd.read_html(url)[0]
            playerstats = playerstats.T.reset_index().T
            
            # Fix column names
            playerstats.loc['level_0',playerstats.loc['level_0',:].str.contains("Unnamed")] = ""  # replace ugly names with blanks
            colnames = [playerstats.loc['level_1',:][i] if playerstats.loc['level_0',:][i] == "" 
                        else playerstats.loc['level_0',:][i] + "_" + playerstats.loc['level_1',:][i] 
                        for i in (range(len(playerstats.loc['level_0',:])))] # create colnames list based on previous multiindex
            playerstats.columns = colnames
            playerstats = playerstats.iloc[2:,:]
            playerstats.columns.values[5] = "Location"
            playerstats.columns.values[7] = "Result"
            
            # Remove intermediate header rows and redudant column
            playerstats = playerstats.loc[playerstats['Rk'] != 'Rk',:]
            playerstats = playerstats.iloc[:,1:]
            playerstats = playerstats.reset_index(drop=True)
            
            # Change location column to show H or A for home/away games
            playerstats['Location'] = ["A" if playerstats['Location'][i] == "@" else "H"
                                       for i in range(len(playerstats['Location']))]
            
            # Show only year of age instead of age + days
            playerstats['Age'] = playerstats['Age'].str[:2]
            
            # Convert columns to correct data types
            playerstats = playerstats.convert_dtypes()
            playerstats = playerstats.apply(pd.to_numeric, errors='ignore')
        
            timesplit = playerstats['TOI'].str.split(':',expand=True).apply(pd.to_numeric)
            playerstats['TOI'] = timesplit[0] + timesplit[1]/60
            
            # Save season data to csv
            playerstats.to_csv(basedirectory +'/'+str(season)+'.csv',index =False)
            
            # Save player image if it doesnt exist
            if not os.path.exists('www/playerimages/'+ player +'.jpg'):
                page = requests.get(url)
                soup = BeautifulSoup(page.text, 'lxml')
                
                if len(soup.find_all('img',{"alt" : "Photo of "})) == 1:
                    imgurl = soup.find_all('img',{"alt" : "Photo of "})[0].get('src')
                    img = open('www/playerimages/'+ player +'.jpg','wb')
                    img.write(requests.get(imgurl).content)
                    img.close()

def getPlayerNames():
    # Get list of all playerIDs in data folder
    playerIDs = next(os.walk("Data/Players/"))[1]
    
    # Get list of existing data in PlayerNames file
    existingData = pd.read_csv("Data/PlayerNames.csv")
    existingIDs = list(existingData['ID'])
    
    # Find player name from website and save to csv
    playerNames = list(existingData['Name'])
    playerPositions = list(existingData['Position'])
    for player in playerIDs:
        if player not in existingIDs:
            print("Getting player info for",player)
            url = "https://www.hockey-reference.com/players/"+player[0]+"/"+player+".html"
            page = requests.get(url)
            soup = BeautifulSoup(page.text, 'lxml')
            metadata = soup.find_all('div',id = 'meta')
            playerNames.append(metadata[0].find('span').text)
            position = metadata[0].find('p').text
            position = position.replace("\n", " ")
            position = position.replace("\xa0", " ")
            position = re.search(' (.*?) ', position).group(1)
            playerPositions.append(position)
            existingIDs.append(player)
            
    playerNamesDF = pd.DataFrame(playerNames)
    playerNamesDF.columns = ["Name"]
    playerNamesDF['Position'] = playerPositions
    playerNamesDF['ID'] = existingIDs
    
    # Standardize positions
    playerNamesDF.loc[playerNamesDF['Position']=='W','Position'] = 'RW'
    playerNamesDF.loc[playerNamesDF['Position']=='F','Position'] = 'C'
    playerNamesDF.loc[playerNamesDF['Position']=='C/LW','Position'] = 'C'
    playerNamesDF.loc[playerNamesDF['Position']=='C/RW','Position'] = 'C'
    playerNamesDF.loc[playerNamesDF['Position']=='C/W','Position'] = 'C'
    playerNamesDF.loc[playerNamesDF['Position']=='RW/C','Position'] = 'RW'
    playerNamesDF.loc[playerNamesDF['Position']=='LW/C','Position'] = 'LW'
    playerNamesDF.loc[playerNamesDF['Position']=='D/LW','Position'] = 'D'
    playerNamesDF.loc[playerNamesDF['Position']=='D/RW','Position'] = 'D'
    playerNamesDF.loc[playerNamesDF['Position']=='D/W','Position'] = 'D'
    
    # Add number to duplicated names
    dup = dict(Counter(playerNamesDF['Name']))
    l_uniq = unique(playerNamesDF['Name'],return_index=True)
    names = [name if i == 0 else name + ' (' + str(i+1) + ')' for name in playerNamesDF['Name'][sorted(l_uniq[1])] for i in range(dup[name])]
    playerNamesDF['Name'] = names
    
    playerNamesDF.to_csv("Data/PlayerNames.csv",index=False)
        
def mergeSeasonStats(seasons):
    # Get list of all playerIDs in data folder
    playerIDs = next(os.walk("Data/Players/"))[1]
    
    # Get player names and positions
    playerNamesDF = pd.read_csv("Data/PlayerNames.csv")

    
    # If single season is used as input, convert to list of length 1
    if type(seasons) is int:
        seasons = [seasons]
        
    for season in seasons:
        print("Merging player stats for",season)
        mergedDataSkaters = pd.DataFrame()
        mergedDataGoalies = pd.DataFrame()
        for player in playerIDs:
            if (playerNamesDF['Position'].loc[playerNamesDF['ID']==player] != "G").iloc[0]:
                
                # Data directory
                datadir = "Data/Players/"+player+"/"+str(season)+".csv"
                
                # Check if player played this year
                if os.path.exists(datadir):
                    playerdata = pd.read_csv("Data/Players/"+player+"/"+str(season)+".csv")
                    
                    # Get season summary for this player
                    summarydata = {
                        "ID": player,
                        "Age": playerdata['Age'].max(),
                        "GP": playerdata.shape[0],
                        "TOI Total": playerdata['TOI'].sum(),
                        "Goals": playerdata['Scoring_G'].sum(),
                        "Assists": playerdata['Scoring_A'].sum(),
                        "Points": playerdata['Scoring_PTS'].sum(),
                        "PP Goals": playerdata['Goals_PP'].sum(),
                        "PP Assists": playerdata['Assists_PP'].sum(),
                        "+/-": playerdata['+/-'].sum(),
                        "Shots": playerdata['S'].sum(),
                        "Shot %": playerdata['S%'].mean(),
                        "FOW": playerdata['FOW'].sum(),
                        "FO %": playerdata['FOW'].mean(),
                        "Hits": playerdata['HIT'].sum(),
                        "Blocks": playerdata['BLK'].sum(),
                        "PIM": playerdata['PIM'].sum(),
                        
                    }
                    summarydata = pd.Series(summarydata).to_frame().T
                    mergedDataSkaters = mergedDataSkaters.append(summarydata)
            
               
                
            else:
                # Data directory
                datadir = "Data/Players/"+player+"/"+str(season)+".csv"
                
                # Check if player played this year
                if os.path.exists(datadir):
                    playerdata = pd.read_csv("Data/Players/"+player+"/"+str(season)+".csv")
                    
                    # Get season summary for this player
                    summarydata = {
                        "ID": player,
                        "Age": playerdata['Age'].max(),
                        "GP": playerdata.shape[0],
                        "Wins": playerdata.loc[playerdata['DEC']=="W"].shape[0],
                        "GA": playerdata['Goalie Stats_GA'].sum(),
                        "SA": playerdata['Goalie Stats_SA'].sum(),
                        "SV %": playerdata['Goalie Stats_SV'].sum()/playerdata['Goalie Stats_SA'].sum(),
                        "SO": playerdata['Goalie Stats_SO'].sum()
                    }
                    summarydata = pd.Series(summarydata).to_frame().T
                    mergedDataGoalies = mergedDataGoalies.append(summarydata)
            
            
        # Save to csv
        os.makedirs("Data/allSkaters/", exist_ok=True)
        os.makedirs("Data/allGoalies/", exist_ok=True)
        mergedDataSkaters.to_csv("Data/allSkaters/"+str(season)+".csv",index=False)
        mergedDataGoalies.to_csv("Data/allGoalies/"+str(season)+".csv",index=False)
        
def getPlayerLines():
    # Get URL for each team
    url = "https://www.dailyfaceoff.com/teams/"
    agent = {"User-Agent":"Mozilla/5.0"}
    page = requests.get(url,headers=agent)
    soup = BeautifulSoup(page.text, 'lxml')
    teams = soup.find_all('a',{"class": "team-logo-img"})
    teams = [i['href'] for i in teams]
    
    # Get playerIDs
    playerIDs = pd.read_csv('Data/PlayerNames.csv')
    
    # Loop through each team
    allPlayerLines = pd.DataFrame()
    for team in teams:
        urlteam = url+team 
        page = requests.get(urlteam,headers=agent)
        soup = BeautifulSoup(page.text, 'lxml')
        
        # Get forward lines
        for i in [1,2,3,4]:
            LW = soup.find_all('td',id='LW'+str(i))[0]
            LW = LW.find('span',{'class':'player-name'}).text
            #LWID = playerIDs.loc[playerIDs['Name'].str.lower()==str.lower(LW),'ID'].iloc[0]
            RW = soup.find_all('td',id='RW'+str(i))[0]
            RW = RW.find('span',{'class':'player-name'}).text
            #RWID = playerIDs.loc[playerIDs['Name'].str.lower()==str.lower(RW),'ID'].iloc[0]
            C = soup.find_all('td',id='C'+str(i))[0]
            C = C.find('span',{'class':'player-name'}).text
            #CID = playerIDs.loc[playerIDs['Name'].str.lower()==str.lower(C),'ID'].iloc[0]
            
            LWLine = pd.DataFrame({'Player':LW,'Linemate1':C,'Linemate2':RW},index = [0])
            CLine = pd.DataFrame({'Player':C,'Linemate1':LW,'Linemate2':RW},index = [0])
            RWLine = pd.DataFrame({'Player':RW,'Linemate1':C,'Linemate2':LW},index = [0])

            allPlayerLines = pd.concat([allPlayerLines,LWLine,CLine,RWLine],ignore_index=True)
        
# Execute functions
currentSeason = fiscalyear.FiscalYear.current().fiscal_year
pullPlayerStats(currentSeason,"skaters")
pullPlayerStats(currentSeason,"goalies")
getPlayerNames()
mergeSeasonStats(currentSeason)
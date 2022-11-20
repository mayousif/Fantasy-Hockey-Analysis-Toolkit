import pandas as pd
import numpy as np
import requests
from bs4 import BeautifulSoup
import os
import fiscalyear
from lxml.html import fromstring
from itertools import cycle
from io import StringIO
from datetime import datetime, timedelta
import time
import unicodedata
fiscalyear.START_MONTH = 10
currentSeason = fiscalyear.FiscalYear.current().fiscal_year


def get_proxies(testurl):
    url = 'https://free-proxy-list.net/'
    response = requests.get(url)
    parser = fromstring(response.text)
    proxies = set()
    for i in parser.xpath('//tbody/tr')[:100]:
        if i.xpath('.//td[7][contains(text(),"yes")]'):
            proxy = ":".join([i.xpath('.//td[1]/text()')[0], i.xpath('.//td[2]/text()')[0]])
            proxies.add(proxy)

    proxy_pool = cycle(proxies)
    goodproxies = []
    for i in range(1,201):
        #Get a proxy from the pool
        proxy = next(proxy_pool)
        print("Request #%d"%i)
        try:
            response = requests.get(testurl, proxies={'http': f"http://{proxy}",'https': f"http://{proxy}"},timeout = 0.5) 
            if response.status_code == 200:
                goodproxies.append(proxy)
            print(response.status_code) 
        except:
            #Most free proxies will often get connection errors. You will have retry the entire request using another proxy to work. 
            #We will just skip retries as its beyond the scope of this tutorial and we are only downloading a single url 
            print("Skipping. Connnection error")
            
    return(goodproxies)



# Pull stats for all players of given type ("skaters" or "goalies")
# for all given seasons (e.g., 2022)
def pullPlayerStats(seasons,playerType):
    
    # Get proxy list
    #proxies = get_proxies('https://www.hockey-reference.com/')
    headers = {'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36'}

    # If single season is used as input, convert to list of length 1
    if type(seasons) is not list:
        seasons = [seasons]
    
    # Iterate through all seasons
    for season in seasons:
        
        # Read in player stats for the season
        url = "https://www.hockey-reference.com/leagues/NHL_"+str(season)+"_"+playerType+".html"
        
        # Use beatifulsoup to extract unique player IDs and stats table
        page = requests.get(url)
        soup = BeautifulSoup(page.text, 'lxml')
        table1 = soup.find('table', id='stats')
        playerIDs = []
        for i in table1.find_all('td'):
            if i.has_attr('data-append-csv'):
                playerIDs.append(i.get('data-append-csv'))
        table1_df = pd.read_html(str(table1))[0]
        table1_df = table1_df.droplevel(0,axis=1)
        table1_df = table1_df.loc[table1_df['Rk'].str.isnumeric(),:]
        table1_df['ID'] = playerIDs
        
        
        # Get locally stored data to cross-reference
        if (playerType =="skaters"):
            currentData = pd.read_csv("Data/allSkaters/"+str(currentSeason)+".csv")
        else:
            currentData = pd.read_csv("Data/allGoalies/"+str(currentSeason)+".csv")
        # Iterate through all players that played this season
        for player in playerIDs:
            if (sum(currentData['ID']==player)!=0):
                localGP = currentData['GP'].loc[currentData['ID']==player].iloc[0]
            else:
                localGP = 0   
            websiteGP = table1_df['GP'].loc[table1_df['ID']==player].iloc[0]
            if (str(localGP) != str(websiteGP)):
                time.sleep(5)
                print("Getting",player, "stats for",season)
                
                # Create folders to store player data if they dont exist
                basedirectory = "Data/Players/" + player
                os.makedirs(basedirectory, exist_ok=True)
                
                # Get player game log for current season
                url = "https://www.hockey-reference.com/players/"+player[0]+"/"+player+"/gamelog/"+str(season)
                attempts = 0
                while attempts <=3:
                    try:
                        #proxy = random.choice(proxies)
                        #proxy_Dict = { 'http' : 'http://'+proxy,
                        #               'https' : 'https://'+proxy,
                        # }
                        r = requests.get(url,headers = headers).text
                        break
                    except:
                        attempts += 1
                playerstats = pd.read_html(StringIO(r))[0]
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

def getPlayerNames(seasons):
    
    # Get list of all playerIDs in data folder
    localPlayerIDs = next(os.walk("Data/Players/"))[1]
    
    playerNamesDF = pd.DataFrame({"ID":localPlayerIDs})
    playerNamesDF['Name'] = np.nan
    playerNamesDF['Position'] = np.nan
    
    # If single season is used as input, convert to list of length 1
    if type(seasons) is int:
        seasons = [seasons]
        
    for season in list(reversed(seasons)):
        print("Getting playernames for "+str(season))
        for playerType in ["skaters","goalies"]:
            time.sleep(4)
            url = "https://www.hockey-reference.com/leagues/NHL_"+str(season)+"_"+playerType+".html"
            
            # Store table text in dataframe
            playertable = pd.read_html(url)[0]
            playertable = playertable.droplevel(0,axis = 1)
            playertable = playertable[playertable['Rk'] != 'Rk']
            
            # Get embedded player ids from table
            response = requests.get(url)
            soup = BeautifulSoup(response.text, 'html.parser')
            table = soup.find('table')
            
            playerIDs = []
            for tr in table.findAll("tr"):
                trs = tr.findAll("td")
                try:
                    playerID = trs[0].find('a')['href'].rsplit("/")[3]
                    playerID = playerID[:-5]
                    playerIDs.append(playerID)
                except:
                    pass
                
            # Clean up player table and append to playernames DF
            playertable['ID'] = playerIDs
            if playerType == "goalies":
                playertable['Pos'] = "G"
            playertable["Player"] = playertable["Player"].str.title()
            
            # If player played for multiple teams, keep last team played for
            playertable = playertable.loc[playertable['Tm']!= "TOT"]
            playertable = playertable.drop_duplicates(subset=['ID'], keep='first')
           
            
            # Append team abv to playername if currently playing this season
            if season == currentSeason:
                playertable['Tm'] = playertable['Tm'].replace("MTL","MON",regex=True)
                playertable['Player'] = playertable['Player'] +" ("+playertable['Tm']+")"
            
            playertable = playertable.loc[:,['Player','Pos','ID']]
            playertable = playertable.set_axis(['Name','Position','ID'],axis=1)
            playertable.loc[playertable['Position']=='W','Position'] = 'RW'
            playertable.loc[playertable['Position']=='F','Position'] = 'C'
            playertable = playertable.set_index('ID')
            
            playerNamesDF = playerNamesDF.set_index("ID").combine_first(playertable).reset_index()
        
    # Convert all characters to ASCII standard
    playerNamesDF = playerNamesDF.astype(str)
    playerNamesDF["Name"] = playerNamesDF["Name"].apply(lambda val: unicodedata.normalize('NFKD', val).encode('ascii', 'ignore').decode())
    playerNamesDF = playerNamesDF.replace('\\*', '',regex=True)
    playerNamesDF = playerNamesDF.replace('Tim Stutzle', 'Tim Stuetzle',regex=True)
    playerNamesDF = playerNamesDF.replace('Mitch Marner', 'Mitchell Marner',regex=True)
    playerNamesDF = playerNamesDF.replace('Matthew Beniers', 'Matty Beniers',regex=True)
    playerNamesDF = playerNamesDF.replace('Calvin Petersen', 'Cal Petersen',regex=True)
    playerNamesDF = playerNamesDF.replace('Anthony Deangelo', 'Tony Deangelo',regex=True)
    playerNamesDF = playerNamesDF.replace('Joshua Norris', 'Josh Norris',regex=True)
    playerNamesDF = playerNamesDF.replace('Michael Anderson', 'Mikey Anderson',regex=True)
    playerNamesDF = playerNamesDF.replace('William Borgen', 'Will Borgen',regex=True)
    playerNamesDF = playerNamesDF.replace('Mathew Dumba', 'Matt Dumba',regex=True)
    playerNamesDF = playerNamesDF.replace('Callan Foote', 'Cal Foote',regex=True)
    playerNamesDF = playerNamesDF.replace('Zachary Jones', 'Zac Jones',regex=True)
    playerNamesDF = playerNamesDF.replace('Axel Jonsson Fjallby', 'Axel Jonsson-Fjallby',regex=True)
    playerNamesDF = playerNamesDF.replace('Zachary Jones', 'Zac Jones',regex=True)
    playerNamesDF = playerNamesDF.replace('Jonathon Merrill', 'Jon Merrill',regex=True)
    playerNamesDF = playerNamesDF.replace('Matthew Nieto', 'Matt Nieto',regex=True)
    playerNamesDF = playerNamesDF.replace('Nick Paul', 'Nicholas Paul',regex=True)
    playerNamesDF = playerNamesDF.replace('Nicklaus Perbix', 'Nick Perbix',regex=True)
    playerNamesDF = playerNamesDF.replace('John-Jason Peterka ', 'Jj Peterka ',regex=True)
    playerNamesDF = playerNamesDF.replace('Samuel Poulin', 'Sam Poulin',regex=True)
    playerNamesDF = playerNamesDF.replace('Alexander Wennberg', 'Alex Wennberg',regex=True)
    playerNamesDF = playerNamesDF.replace('Yegor Zamula', 'Egor Zamula',regex=True)
    
    # Save to file
    playerNamesDF.to_csv("Data/PlayerNames.csv",index=False)



# Merge stats with optional paramater to get last 7,14, or 30 day stats
# (optional time parameters are "7day","14day",or "30day")
def mergeSeasonStats(seasons,time = ""):
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
                    playerdata['Date'] = pd.to_datetime(playerdata['Date'])
                    if time == "7day":
                        startDate = pd.Timestamp.now() - timedelta(days=7)
                        playerdata = playerdata[playerdata['Date']>=startDate]
                    elif time == "14day":
                        startDate = pd.Timestamp.now() - timedelta(days=14)
                        playerdata = playerdata[playerdata['Date']>=startDate]
                    elif time == "30day":
                        startDate = pd.Timestamp.now() - timedelta(days=30)
                        playerdata = playerdata[playerdata['Date']>=startDate]
                        
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
                        "FOL": playerdata['FOL'].sum(),
                        "FO %": playerdata['FOW'].mean(),
                        "Hits": playerdata['HIT'].sum(),
                        "Blocks": playerdata['BLK'].sum(),
                        "PIM": playerdata['PIM'].sum(),
                        "SH Goals": playerdata['Goals_SH'].sum(),
                        "SH Assists": playerdata['Assists_SH'].sum(),
                        "GWG": playerdata['Goals_GW'].sum()
                        
                    }
                    summarydata = pd.Series(summarydata).to_frame().T
                    mergedDataSkaters = mergedDataSkaters.append(summarydata)
            
               
                
            else:
                # Data directory
                datadir = "Data/Players/"+player+"/"+str(season)+".csv"
                
                # Check if player played this year
                if os.path.exists(datadir):
                    playerdata = pd.read_csv("Data/Players/"+player+"/"+str(season)+".csv")
                    playerdata['Date'] = pd.to_datetime(playerdata['Date'])
                    if time == "7day":
                        startDate = pd.Timestamp.now() - timedelta(days=7)
                        playerdata = playerdata[playerdata['Date']>startDate]
                    elif time == "14day":
                        startDate = pd.Timestamp.now() - timedelta(days=14)
                        playerdata = playerdata[playerdata['Date']>=startDate]
                    elif time == "30day":
                        startDate = pd.Timestamp.now() - timedelta(days=30)
                        playerdata = playerdata[playerdata['Date']>=startDate]
                    
                    
                    # Get season summary for this player
                    summarydata = {
                        "ID": player,
                        "Age": playerdata['Age'].max(),
                        "GP": playerdata.shape[0],
                        "Wins": playerdata.loc[playerdata['DEC']=="W"].shape[0],
                        "Losses": playerdata.loc[playerdata['DEC']=="L"].shape[0],
                        "GA": playerdata['Goalie Stats_GA'].sum(),
                        "SA": playerdata['Goalie Stats_SA'].sum(),
                        "SV %": playerdata['Goalie Stats_SV'].sum()/playerdata['Goalie Stats_SA'].sum(),
                        "SO": playerdata['Goalie Stats_SO'].sum(),
                        
                    }
                    summarydata = pd.Series(summarydata).to_frame().T
                    mergedDataGoalies = mergedDataGoalies.append(summarydata)
            
        # Save to csv
        os.makedirs("Data/allSkaters/", exist_ok=True)
        os.makedirs("Data/allGoalies/", exist_ok=True)
        if time == "7day":
            mergedDataSkaters.to_csv("Data/allSkaters/7day.csv",index=False)
            mergedDataGoalies.to_csv("Data/allGoalies/7day.csv",index=False)
        elif time == "14day":
            mergedDataSkaters.to_csv("Data/allSkaters/14day.csv",index=False)
            mergedDataGoalies.to_csv("Data/allGoalies/14day.csv",index=False)
        elif time == "30day":
            mergedDataSkaters.to_csv("Data/allSkaters/30day.csv",index=False)
            mergedDataGoalies.to_csv("Data/allGoalies/30day.csv",index=False)  
        else:
            mergedDataSkaters.to_csv("Data/allSkaters/"+str(season)+".csv",index=False)
            mergedDataGoalies.to_csv("Data/allGoalies/"+str(season)+".csv",index=False)
        
def getPlayerLines():
    # Get URL for each team
    url = "https://www.dailyfaceoff.com/teams/"
    headers = {'user-agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/86.0.4240.75 Safari/537.36'}
    page = requests.get(url,headers=headers)
    soup = BeautifulSoup(page.text, 'lxml')
    teams = soup.find_all('a',{"class": "team-logo-img"})
    teams = [i['href'] for i in teams]
    
    # Read in NHL teams names + abv
    teamnames = pd.read_csv("Data/teamNames.csv")
    
    # Loop through each team
    allPlayerLines = pd.DataFrame()
    for team in teams:
        print("Getting updated player lines for "+team)
        time.sleep(4)
        teamname = team.split("/")[0].replace("-"," ").title()
        teamabv = teamnames['teamabv'].loc[teamnames['teamname']==teamname].iloc[0]
        urlteam = url+team 
        page = requests.get(urlteam,headers=headers)
        soup = BeautifulSoup(page.text, 'lxml')
        
        # Get forward lines
        for i in [1,2,3,4]:
            if (soup.find_all('td',id='LW'+str(i))!=[]):
                LW = soup.find_all('td',id='LW'+str(i))[0]
                if not LW.find('span',{'class':'player-name'}) is None:
                    LW = LW.find('span',{'class':'player-name'}).text.title()
                    LW = LW + " ("+teamabv+")"
                else:
                    LW = ""
            else:
                LW = ""
            if (soup.find_all('td',id='RW'+str(i))!=[]):
                RW = soup.find_all('td',id='RW'+str(i))[0]
                if not RW.find('span',{'class':'player-name'}) is None:
                    RW = RW.find('span',{'class':'player-name'}).text.title()
                    RW = RW + " ("+teamabv+")"
                else:
                    RW = ""
            else:
                RW = ""
            if (soup.find_all('td',id='C'+str(i))!=[]):
                C = soup.find_all('td',id='C'+str(i))[0]
                if not C.find('span',{'class':'player-name'}) is None:
                    C = C.find('span',{'class':'player-name'}).text.title()
                    C = C + " ("+teamabv+")"
                else:
                    C = ""
            else:
                C = ""
        
            if LW != "":
                LWLine = pd.DataFrame({'Player':LW,'Linemate1':C,'Linemate2':RW,'Line':i},index = [0])
                allPlayerLines = pd.concat([allPlayerLines,LWLine],ignore_index=True)
            if C != "":  
                CLine = pd.DataFrame({'Player':C,'Linemate1':LW,'Linemate2':RW,'Line':i},index = [0])
                allPlayerLines = pd.concat([allPlayerLines,CLine],ignore_index=True)
            if RW != "": 
                RWLine = pd.DataFrame({'Player':RW,'Linemate1':C,'Linemate2':LW,'Line':i},index = [0])
                allPlayerLines = pd.concat([allPlayerLines,RWLine],ignore_index=True)
            
        
        # Get defense pairs
        for i in [1,2,3]:
            if (soup.find_all('td',id='LD'+str(i))!=[]):
                LD = soup.find_all('td',id='LD'+str(i))[0]
                if not LD.find('span',{'class':'player-name'}) is None:
                    LD = LD.find('span',{'class':'player-name'}).text.title()
                    LD = LD + " ("+teamabv+")"
                else:
                    LD = ""
            else:
                LD = ""
            if (soup.find_all('td',id='RD'+str(i))!=[]):
                RD = soup.find_all('td',id='RD'+str(i))[0]
                if not RD.find('span',{'class':'player-name'}) is None:
                    RD = RD.find('span',{'class':'player-name'}).text.title()
                    RD = RD + " ("+teamabv+")"
                else:
                    RD = ""
            else:
                RD = ""
            
            if LD != "":
                LDLine = pd.DataFrame({'Player':LD,'Linemate1':RD,'Linemate2':'','Line':i},index = [0])
                allPlayerLines = pd.concat([allPlayerLines,LDLine],ignore_index=True)
            if RD != "":
                RDLine = pd.DataFrame({'Player':RD,'Linemate1':LD,'Linemate2':'','Line':i},index = [0])
                allPlayerLines = pd.concat([allPlayerLines,RDLine],ignore_index=True)
        
        # Get PP1 and PP2 players
        for i in [1,2]:
            PPLW = soup.find_all('td',id='PPLW'+str(i))[0]
            PPLW = PPLW.find('span',{'class':'player-name'}).text.title()
            PPLW = PPLW + " ("+teamabv+")"
            PPRW = soup.find_all('td',id='PPRW'+str(i))[0]
            PPRW = PPRW.find('span',{'class':'player-name'}).text.title()
            PPRW = PPRW + " ("+teamabv+")"
            PPC = soup.find_all('td',id='PPC'+str(i))[0]
            PPC = PPC.find('span',{'class':'player-name'}).text.title()
            PPC = PPC + " ("+teamabv+")"
            PPLD = soup.find_all('td',id='PPLD'+str(i))[0]
            PPLD = PPLD.find('span',{'class':'player-name'}).text.title()
            PPLD = PPLD + " ("+teamabv+")"
            PPRD = soup.find_all('td',id='PPRD'+str(i))[0]
            PPRD = PPRD.find('span',{'class':'player-name'}).text.title()
            PPRD = PPRD + " ("+teamabv+")"
            
            PPLine = [PPLW,PPC,PPRW,PPLD,PPRD]
            
            allPlayerLines.loc[allPlayerLines['Player'].isin(PPLine), 'PP'] = i
    
    # Convert all characters to ASCII standard
    allPlayerLines['Player'] = allPlayerLines['Player'].apply(lambda val: unicodedata.normalize('NFKD', val).encode('ascii', 'ignore').decode())
    allPlayerLines['Linemate1'] = allPlayerLines['Linemate1'].apply(lambda val: unicodedata.normalize('NFKD', val).encode('ascii', 'ignore').decode())
    allPlayerLines['Linemate2'] = allPlayerLines['Linemate2'].apply(lambda val: unicodedata.normalize('NFKD', val).encode('ascii', 'ignore').decode())
    
    # Replace one-off issues with names
    allPlayerLines = allPlayerLines.replace('Matthew Boldy', 'Matt Boldy',regex=True)
    allPlayerLines = allPlayerLines.replace('Tim Stutzle', 'Tim Stuetzle',regex=True)
    allPlayerLines = allPlayerLines.replace('Matthew Beniers', 'Matty Beniers',regex=True)
    allPlayerLines = allPlayerLines.replace('Mitch Marner', 'Mitchell Marner',regex=True)
    allPlayerLines = allPlayerLines.replace('William Borgen', 'Will Borgen',regex=True)
    allPlayerLines = allPlayerLines.replace('Mathew Dumba', '"Matt Dumba',regex=True)
    allPlayerLines = allPlayerLines.replace('Matthew Nieto', 'Matt Nieto',regex=True)
    allPlayerLines = allPlayerLines.replace('Nick Paul', 'Nicholas Paul',regex=True)
    allPlayerLines = allPlayerLines.replace('John Jason Peterka ', 'Jj Peterka ',regex=True)
    allPlayerLines = allPlayerLines.replace('Alexander Wennberg', 'Alex Wennberg',regex=True)
    allPlayerLines = allPlayerLines.replace('Yegor Zamula', 'Egor Zamula',regex=True)
    
    # Write to csv
    allPlayerLines.to_csv("Data/PlayerLines.csv",index=False)


def pullSeasonSchedule(currentSeason):
    # page URL
    url = "https://www.hockey-reference.com/leagues/NHL_"+str(currentSeason)+"_games.html"
    
    # Season schedule
    seasonschedule = pd.read_html(url)[0]
    seasonschedule['Visitor'] = seasonschedule['Visitor'].str.replace(".","",regex=False)
    seasonschedule['Home'] = seasonschedule['Home'].str.replace(".","",regex=False)
    
    # Append team abv and sort by date
    teamnames = pd.read_csv("Data/teamNames.csv")
    seasonschedule = pd.merge(seasonschedule, teamnames, left_on='Visitor', right_on='teamname')
    seasonschedule = seasonschedule.drop('teamname',axis=1)
    seasonschedule = seasonschedule.rename(columns={"teamabv": "VisABV"})
    seasonschedule = pd.merge(seasonschedule, teamnames, left_on='Home', right_on='teamname')
    seasonschedule = seasonschedule.drop('teamname',axis=1)
    seasonschedule = seasonschedule.rename(columns={"teamabv": "HomeABV"})
    seasonschedule['Date'] = pd.to_datetime(seasonschedule['Date'])
    seasonschedule = seasonschedule.sort_values('Date')
    
    # Save to file
    if not os.path.exists("Data/Schedule/"):
        os.makedirs("Data/Schedule/")
    seasonschedule.to_csv("Data/Schedule/"+str(currentSeason)+".csv",index=False)
    


# Execute functions
pullPlayerStats(currentSeason,"skaters")
pullPlayerStats(currentSeason,"goalies")
mergeSeasonStats(currentSeason)
mergeSeasonStats(currentSeason,time="7day")
mergeSeasonStats(currentSeason,time="14day")
mergeSeasonStats(currentSeason,time="30day")
getPlayerNames(list(range(2010,currentSeason+1)))
getPlayerLines()
pullSeasonSchedule(currentSeason)
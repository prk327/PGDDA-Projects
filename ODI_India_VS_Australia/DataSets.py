import glob
import pandas as pd
import yaml

path =r'/home/prabhakar/Downloads/odis_male' # use your path
allFiles = glob.glob(path + "/*.yaml")

list_ = []

for file_ in allFiles:
    with open(file_, 'r') as stream:
        try:
            ODI = yaml.load(stream)
        except yaml.YAMLError as exc:
            print(exc)
    if 'INDIA' in [x.strip().upper() for x in ODI['info']['teams']] and 'AUSTRALIA' in [x.strip().upper() for x in ODI['info']['teams']]:
        team = []
        batsman = []
        bowler = []
        batsman_runs = []
        extra_runs = []
        total_runs = []
        fielder = []
        outby = []
        player_out = []
        wicket = []
        sixes = []
        fours = []
        innings = []
        for i, j in enumerate(ODI['innings']):
            for k in ODI['innings'][i]:
                for l,m in enumerate(ODI['innings'][i][k]['deliveries']):
                    for n in list(ODI['innings'][i][k]['deliveries'][l].keys()):
                        innings.append(k)
                        team.append(ODI['innings'][i][k]['team'])
                        batsman.append(ODI['innings'][i][k]['deliveries'][l][n]['batsman'])
                        bowler.append(ODI['innings'][i][k]['deliveries'][l][n]['bowler'])
                        batsman_runs.append(ODI['innings'][i][k]['deliveries'][l][n]['runs']['batsman'])
                        extra_runs.append(ODI['innings'][i][k]['deliveries'][l][n]['runs']['extras'])
                        total_runs.append(ODI['innings'][i][k]['deliveries'][l][n]['runs']['total'])
                        try:
                            fielder.append(ODI['innings'][i][k]['deliveries'][l][n]['wicket']['fielders'][0])
                            outby.append(ODI['innings'][i][k]['deliveries'][l][n]['wicket']['kind'])
                            player_out.append(ODI['innings'][i][k]['deliveries'][l][n]['wicket']['player_out'])
                            wicket.append(1)
                        except:
                            fielder.append("None")
                            outby.append("None")
                            player_out.append("None")
                            wicket.append(0)
                        if ODI['innings'][i][k]['deliveries'][l][n]['runs']['batsman'] == 6:
                            sixes.append(1)
                        else:
                            sixes.append(0)
                        if ODI['innings'][i][k]['deliveries'][l][n]['runs']['batsman'] == 4:
                            fours.append(1)
                        else:
                            fours.append(0)
        ODI_International = {'Innings': innings,
                        'Team': team,
                        'Batsman': batsman,
                        'Bowler': bowler,
                        'Batsman_Runs': batsman_runs,
                        'Extra_Runs': extra_runs,
                        'Total_Runs': total_runs,
                        'Fielder': fielder,
                        'OutBy': outby,
                        'Player_Out': player_out,
                        'Wicket': wicket,
                        'Sixes': sixes,
                        'Fours': fours}
        data = pd.DataFrame.from_dict(ODI_International)
        df = data.groupby(['Innings','Team','Batsman','Bowler','OutBy','Fielder','Player_Out'],as_index=False).sum()
        try:
            df['City'] = ODI['info']['city']
        except:
            df['City'] = "Not Found"
        df['Dates'] =  ODI['info']['dates'][0]
        try:
            df['Winner'] =  ODI['info']['outcome']['winner']
        except:
            df['Winner'] = "No Result"
        try:
            df['Man Of The Match'] = ODI['info']['player_of_match'][0]
        except:
            df['Man Of The Match'] = "No One"
        df['Toss_Decision'] = ODI['info']['toss']['decision']
        df['Toss_Winner'] =  ODI['info']['toss']['winner']
        df['Venue'] =  ODI['info']['venue']
        list_.append(df)
        
frame = pd.concat(list_, axis = 0, ignore_index = True)







race.vars <-c("B03002_001E","B03002_003E","B03002_004E","B03002_005E","B03002_006E","B03002_007E","B03002_008E","B03002_009E","B03002_012E","B03002_001M","B03002_003M","B03002_004M","B03002_005M","B03002_006M","B03002_007M","B03002_008M","B03002_009M","B03002_012M")
race.names <-c("pop", "white","black","native","asian","island","other","two","hisp","popMOE", "whiteMOE","blackMOE","nativeMOE","asianMOE","islandMOE","otherMOE","twoMOE","hispMOE")
raceRaw <- np.pull(variables=race.vars, names=race.names) 
save(raceRaw, file = "inputs/raceRaw.RData")

raceOPRaw <- np.pull.op(variables=race.vars, names=race.names)
raceUSRaw <- np.pull.us(variables=race.vars, names=race.names)
save(raceOPRaw, file = "inputs/raceOPRaw.RData")
save(raceUSRaw, file = "inputs/raceUSRaw.RData")

## comparison
race2000.vars <-c("P008001","P008003","P008004","P008005","P008006","P008007","P008008","P008009","P008010")
race2000.names <-c("pop","white","black","native","asian","island","other","two","hisp")
race2000Raw <- np.pull(variables=race2000.vars, names=race2000.names, year = 2000, survey = "sf1") 
save(race2000Raw, file = "inputs/race2000Raw.RData")

raceOP2000Raw <- np.pull.op(variables=race2000.vars, names=race2000.names, year = 2000, survey = "sf1")
raceUS2000Raw <- np.pull.us(variables=race2000.vars, names=race2000.names, year = 2000, survey = "sf1", geo = "state:*") %>% filter(geo != "72")
save(raceOP2000Raw, file = "inputs/raceOP2000Raw.RData")
save(raceUS2000Raw, file = "inputs/raceUS2000Raw.RData")
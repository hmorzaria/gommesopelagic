#' @title Estimate net benefit
#' @description  Calcuate net benefit
#' @details INPUT: 1) Biomass data, Catch data, Catch by fleet
#' @details OUTPUT: 1) Gross benefit , 2) Discounted Net benefit
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com
#' @date August 2020



#calculate Gross benefit

fleets.biomass$gross = fleets.biomass$catch * fleets.biomass$price
fleets.biomass$scenario = sc.list[ScenarioIndex]


#calculate costs

costs = data.frame(c(cost.artisanal, cost.industrial))
colnames(costs) = c("costrate")
costs$Fishery_type=c("Artisanal", "Industrial")

fleets.biomass <- merge(fleets.biomass, costs, by = "Fishery_type")


# net benefit
fleets.biomass$NBt =  fleets.biomass$gross * (1 - fleets.biomass$costrate)
write.csv(fleets.biomass,file=paste("catch_value",sc.list[ScenarioIndex],".csv"))

NBt <- aggregate(fleets.biomass$NBt, list(fleets.biomass$Time), FUN = sum)

# agregate NB by groups to determine what groups contribute more to Net benefit
NBt.groups <- aggregate(fleets.biomass$NBt, list(fleets.biomass$Time,fleets.biomass$group), FUN = sum)
names(NBt.groups) = c("year","group","NBt_group")
NBt.time = NBt
names(NBt.time) = c("year","NBt_tot")
NBt.groups = merge(NBt.time, NBt.groups, by = "year")
NBt.groups$ratio = NBt.groups$NBt_group/NBt.groups$NBt_tot
NBt.groups.2 = NBt.groups[-which(NBt.groups$ratio=="NaN"),]
NBt.groups.time <- aggregate(NBt.groups.2$ratio, list(NBt.groups.2$group), FUN = mean)
print(NBt.groups.time[order(NBt.groups.time$x), ])


NBt.shrimp <- subset(NBt.groups, group%in% c("BD", "PWN"))
NBt.shrimp = drop.levels(NBt.shrimp)
NBt.shrimp = aggregate(NBt.shrimp$NBt_group, list(NBt.shrimp$year), FUN = sum)

NBt <- as.data.frame(NBt)
#names(NBt) <- c("Fishery","Time","NBt")
names(NBt) <- c("Time","NBt")

# Undiscounted benefit
#UB.2038 = NBt.shrimp[31,2]
UB.2038 = NBt[31,2] # 2038
UB.2009 = NBt[2,2] # 2009

write.csv(NBt,file= paste("NBt",sc.list[ScenarioIndex],".csv"))
#write.csv(NBt,file= paste("NBt.groups",sc.list[ScenarioIndex],".csv"))

discount.rate = 1/(1 + interest.rate)
NBt$year = -1:29
NBt$NPVt = NBt$NBt * (discount.rate^NBt$year)

NPV <- sum(NBt$NPVt)


result.matrix[ScenarioIndex,] <- NPV
undiscounted.matrix.2038[ScenarioIndex,] <- UB.2038
undiscounted.matrix.2009[ScenarioIndex,] <- UB.2009




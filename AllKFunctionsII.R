##Population creation function. Creates the total population for the game. 
##Players for individual games are randomly selected from this list. 

MakePop<-function(Population, Parameters)
{
  
  Coord<-data.frame(X = 1, Y = 1:Parameters$popdensity)
  for (i in 1:Parameters$popdensity)
  {
    Coord$X[i] <- sample(1:Parameters$cols, 1)
    Coord$Y[i] <- sample(1:Parameters$rows, 1)
  }
  Coord <- unique( Coord[,1:2] )
  
  PopCoord<-Population[sample(nrow(Population),nrow(Coord)),]
  PopCoord<-cbind(PopCoord,Coord)
  return(PopCoord)
}

##Map creation function. Creates the map and populates it. 

MakeMap<-function(PopCoord, TownMap)
{  
  flag<-0
  for (i in 1:nrow(PopCoord))
  {
    for (j in 1:nrow(TownMap))
    { 
      flag[TownMap$X[j]==PopCoord$X[i]]<- 1
      flag[TownMap$Y[j]==PopCoord$Y[i]]<- flag+1
      if (flag==2)
      {
        TownMap$ID[j]<-PopCoord$ID[i]
        TownMap$Flag[j]<-1
        TownMap$Type[j]<-PopCoord$Type[i]
        TownMap$Vulnerable[j]<-PopCoord$Vulnerable[i]
        TownMap$VulnWeight[j]<-PopCoord$VulnWeight[i]
        TownMap$VulnTot[j]<-PopCoord$VulnTot[i]
        TownMap$Criminal[j]<-PopCoord$Criminal[i]
        TownMap$CrimWeight[j]<-PopCoord$CrimWeight[i]
        TownMap$CrimTot[j]<-PopCoord$CrimTot[i]
        TownMap$Suspicious[j]<-PopCoord$Suspicious[i]
        TownMap$SuspWeight[j]<-PopCoord$SuspWeight[i]
        TownMap$SuspTot[j]<-PopCoord$SuspTot[i]
        TownMap$DetainState[j]<-PopCoord$DetainState[i]      
        TownMap$IgnoreHistory[j]<-PopCoord$IgnoreHistory[i]
        TownMap$DetainHistory[j]<-PopCoord$DetainHistory[i] 
      }
      flag<-0
    }
  } 
  return(TownMap)
}

## This function creates a slim version of the State Matrix that 
## can be used in the reinforcement learning engine.

MiniStateMatrix<-function(state, action, StateMatrix)
  {
  reward<-0
  next_state <- state
  for(i in 1:nrow(StateMatrix)){
    if(state == StateMatrix[i,1] && action == StateMatrix[i,2]) {
      next_state <- StateMatrix[i,3]
      reward <- StateMatrix[i,4]
    }
  }
  out <- list("NextState" = next_state, "Reward" = reward)
  return(out)
  
}

### This function compares the detain state to the history to ascertain whether the person
### in that location has already been detained. If they have, the move is not recorded.

CheckHistory<-function(TownMap,StatePosX,StatePosY,StateDet,DetainHistory,Parameters)
{
  StateFlag=1
  if (DetainHistory != "INI")
  {
  Hist<-unlist(strsplit(DetainHistory,"[.]"))
  X=as.numeric(StatePosX)
  Y=as.numeric(StatePosY) 
    for (pp in 1:(length(Hist)/2))
    {
      newX=length(Hist)-2*(pp-1)-1
      newY=length(Hist)-2*(pp-1)
      if (X==as.numeric(Hist[newX]) &&Y==as.numeric(Hist[newY]))
         { StateFlag = 0 }
    }
  }
  return(StateFlag)
    }

### This function creates all possible states for the game.

AssignState<-function(origin,TownMap,Parameters,Actions)
{
  StateMatrix=data.frame(matrix(vector(), 1, 7, dimnames=list(c(), c("State",
              "Action","NextState","Reward","ActionHistory","Time",
              "DetainHistory"))),stringsAsFactors=F)
  EmptyStateMatrix=StateMatrix
  # First set of states.
  for(j in 1:5)  
  {
    StateVect<-EmptyStateMatrix
    if(Actions[j]=="D" && TownMap$DetainState[origin]!="000")
    { 
      StateVect$State<-paste0(sprintf("%02d",TownMap$X[origin]),".",
                              sprintf("%02d",TownMap$Y[origin]),".",
                              sprintf("%02d",Parameters$daylength),".",
                              TownMap$DetainState[origin])
      StateVect$Action<-Actions[j]
      StateVect$ActionHistory="D"
      StateVect$Time=sprintf("%02d",as.integer(Parameters$daylength)-Parameters$detaintime)
      
      StateVect$NextState<-paste0(sprintf("%02d",TownMap$X[origin]),".",
                                  sprintf("%02d",TownMap$Y[origin]),".",sprintf("%02d",TownMap$X[origin]),".",
                                  sprintf("%02d",TownMap$Y[origin]),".",StateVect$Time
                                  ,".",
                                  TownMap$DetainState[origin])
      StateVect$Reward<-TownMap$CrimTot[origin]
      StateVect$DetainHistory<-paste0(sprintf("%02d",TownMap$X[origin]),".",
                                      sprintf("%02d",TownMap$Y[origin]))
    }
    else if(Actions[j]=="N" &&TownMap$Y[origin]!=Parameters$rows)
    {
      StateVect$State<-paste0(sprintf("%02d",TownMap$X[origin]),".",
              sprintf("%02d",TownMap$Y[origin]),".",sprintf("%02d",
              Parameters$daylength),".",
              TownMap$DetainState[origin])
      StateVect$Action<-Actions[j]
      newY=TownMap$Y[origin]+1
      StateVect$Time=sprintf("%02d",Parameters$daylength-Parameters$traveltime)
      StateVect$NextState<-paste0(sprintf("%02d",TownMap$X[origin]),".", 
                                  sprintf("%02d",newY),".",StateVect$Time,".",TownMap$DetainState[origin+1])
      StateVect$ActionHistory<-"M"
      StateVect$Reward<-Parameters$MoveReward
      StateVect$DetainHistory<-"INI"
    }
    else if(Actions[j]=="S" && TownMap$Y[origin]!=1)
    {
      StateVect$State<-paste0(sprintf("%02d",TownMap$X[origin]),".",
                              sprintf("%02d",TownMap$Y[origin]),".",
                              sprintf("%02d",
                                      Parameters$daylength),".",
                              TownMap$DetainState[origin])
      StateVect$Time=sprintf("%02d",Parameters$daylength-Parameters$traveltime)
      StateVect$Action<-Actions[j]
      newY=TownMap$Y[origin]-1
      StateVect$NextState<-paste0(sprintf("%02d",TownMap$X[origin]),".", sprintf("%02d",newY),
                                  ".",StateVect$Time,".",TownMap$DetainState[origin-1])
      StateVect$ActionHistory<-"M"
      StateVect$Reward<-Parameters$MoveReward
      StateVect$DetainHistory<-"INI"
    }
    else if(Actions[j]=="W" && TownMap$X[origin]!=1)
    {
      StateVect$State<-paste0(sprintf("%02d",TownMap$X[origin]),".",
                              sprintf("%02d",TownMap$Y[origin]),".",sprintf("%02d",
                                                                            Parameters$daylength),".",
                              TownMap$DetainState[origin])
      StateVect$Action<-Actions[j]
      StateVect$Time=sprintf("%02d",Parameters$daylength-Parameters$traveltime)
      newX=TownMap$X[origin]-1
      StateVect$NextState<-paste0(sprintf("%02d",newX),".",sprintf("%02d",TownMap$Y[origin]), 
                                  ".",StateVect$Time,".",TownMap$DetainState[origin-Parameters$rows])
      StateVect$ActionHistory<-"M"
      StateVect$Reward<-Parameters$MoveReward
      StateVect$DetainHistory<-"INI"
    }
    else if(Actions[j]=="E"&& TownMap$X[origin]!=Parameters$cols)
    {
      StateVect$State<-paste0(sprintf("%02d",TownMap$X[origin]),".",
                              sprintf("%02d",TownMap$Y[origin]),".",
                              sprintf("%02d",Parameters$daylength),".",
                              TownMap$DetainState[origin])
      StateVect$Time=sprintf("%02d",Parameters$daylength-Parameters$traveltime)
      StateVect$Action<-Actions[j]
      newX=TownMap$X[origin]+1
      StateVect$NextState<-paste0(sprintf("%02d",newX),".",sprintf("%02d",TownMap$Y[origin]),
                                  ".",StateVect$Time, 
                                  ".",TownMap$DetainState[origin+Parameters$rows])
      StateVect$ActionHistory<-"M"
      StateVect$Reward<-Parameters$MoveReward
      StateVect$DetainHistory<-"INI"
    }
  StateMatrix=rbind(StateMatrix,StateVect)    
  }  
  StateMatrix=unique(StateMatrix)
StateMatrix<-StateMatrix[-which(apply(StateMatrix,1,function(x)all(is.na(x)))),]
 rownames(StateMatrix) <- NULL
 ##### Later States
 for(rr in 1:Parameters$daylength)
 {
   StateMatrix2=EmptyStateMatrix
   for(ii in 1:nrow(StateMatrix))
   { 
     if (as.numeric(StateMatrix$Time[ii])>=Parameters$traveltime)
     { 
       StateExp=unlist(strsplit(StateMatrix$NextState[ii],"[.]"))
       StatePosX=StateExp[length(StateExp)-3]
       StatePosY=StateExp[length(StateExp)-2]
       StateDet=StateExp[length(StateExp)]
       DetainHistory=StateMatrix[ii,7]
       
       for(jj in 1:5)  
       {
         if(Actions[jj]=="D" && as.numeric(StateMatrix$Time[ii])>=Parameters$detaintime)
         {
           StateFlag=0
           StateFlag<-CheckHistory(TownMap,StatePosX,StatePosY,StateDet,DetainHistory,
                                   Parameters)
           if (as.numeric(StateFlag) == 1)
           {
             StateVect<-EmptyStateMatrix
             StateVect$State<-StateMatrix$NextState[ii]
             StateVect$Action<-Actions[jj]
             StateVect$ActionHistory<-paste0(StateMatrix[ii,5],".",StateVect$Action)
             StateVect$Time=sprintf("%02d",as.integer(StateMatrix$Time[ii])-
                                      Parameters$detaintime)
             StateVect$DetainHistory=DetainHistory
             for(tt in 1:nrow(TownMap))
             {
               if(as.numeric(StatePosX)==TownMap$X[tt]&&
                  as.numeric(StatePosY)==TownMap$Y[tt])
                 
               {
                 StateVect$Reward=TownMap$CrimTot[tt]
               }}
             if (StateVect$DetainHistory=="INI")
             {
               
               StateVect$DetainHistory=paste0(StatePosX,".",StatePosY)
             }
             else {
               
               StateVect$DetainHistory=paste0(DetainHistory,".",
                                              StatePosX,".",StatePosY)
             }
             StateVect$NextState<-paste0(StateVect$DetainHistory,".",StatePosX,".", 
                                         StatePosY,".",StateVect$Time,".",StateDet)
           }
         }
         else  if(Actions[jj]=="N" && as.numeric(StatePosY)!=Parameters$rows)
         {
           StateVect<-EmptyStateMatrix
           StateVect$State<-StateMatrix$NextState[ii]
           StateVect$Action<-Actions[jj]
           StateVect$DetainHistory<-StateMatrix[ii,7]
           StateVect$Reward<-Parameters$MoveReward
           StateVect$Time=sprintf("%02d", as.integer(StateMatrix$Time[ii])-Parameters$traveltime)
           newY=sprintf("%02d", as.numeric(StatePosY)+1)
           for(tt in 1:nrow(TownMap))
           {
             if(as.numeric(StatePosX)==TownMap$X[tt]&&
                as.numeric(newY)==TownMap$Y[tt])
             {
               StateDet=TownMap$DetainState[tt]
             }}
           StateVect$ActionHistory<-paste0(StateMatrix[ii,5],".M")
           if (StateVect$DetainHistory=="INI")
           {          
             StateVect$NextState<-paste0(StatePosX,".", newY,".",StateVect$Time,
                                         ".",StateDet)
           }
           else
           {          
             StateVect$NextState<-paste0(StateVect$DetainHistory,".", 
                                         StatePosX,".", newY,"."
                                         ,StateVect$Time,
                                         ".",StateDet)
           }
         } 
         else if(Actions[jj]=="S" && as.numeric(StatePosY)!=1)
         {
           StateVect<-EmptyStateMatrix
           StateVect$State<-StateMatrix$NextState[ii]
           StateVect$Action<-Actions[jj]
           StateVect$DetainHistory<-StateMatrix[ii,7]
           StateVect$Reward<-Parameters$MoveReward
           StateVect$Time=sprintf("%02d", as.integer(StateMatrix$Time[ii])-Parameters$traveltime)
           StateVect$ActionHistory<-paste0(StateMatrix[ii,5],".M")
           newY=sprintf("%02d", as.numeric(StatePosY)-1)
           for(tt in 1:nrow(TownMap))
           {
             if(as.numeric(StatePosX)==TownMap$X[tt]&&
                as.numeric(newY)==TownMap$Y[tt])
             {
               StateDet=TownMap$DetainState[tt]
             }}
           if (StateVect$DetainHistory=="INI")
           {          
             StateVect$NextState<-paste0(StatePosX,".", newY,
                                         ".",StateVect$Time,
                                         ".",StateDet)
           }
           else
           {
             StateVect$NextState<-paste0(StateVect$DetainHistory,".", StatePosX,".", newY,
                                         ".",StateVect$Time,
                                         ".",StateDet)
           }            
         }     
         else if(Actions[jj]=="W" && as.numeric(StatePosX)!=1)
         {
           StateVect<-EmptyStateMatrix
           StateVect$ActionHistory<-paste0(StateMatrix[ii,5],".M")
           StateVect$State<-StateMatrix$NextState[ii]
           StateVect$Action<-Actions[jj]
           StateVect$DetainHistory<-StateMatrix[ii,7]
           StateVect$Reward<-Parameters$MoveReward
           StateVect$Time=sprintf("%02d", as.integer(StateMatrix$Time[ii])-Parameters$traveltime)
           newX=sprintf("%02d",as.numeric(StatePosX)-1)
           for(tt in 1:nrow(TownMap))
           {
             if(as.numeric(newX)==TownMap$X[tt]&&
                as.numeric(StatePosY)==TownMap$Y[tt])
             {
               StateDet=TownMap$DetainState[tt]
             }}         
           if (StateVect$DetainHistory=="INI")
           {
             StateVect$NextState<-paste0(newX,".",StatePosY,".",StateVect$Time,
                                         ".",StateDet)
           }
           else
           {
             StateVect$NextState<-paste0(StateVect$DetainHistory,".",
                                         newX,".",StatePosY,".",StateVect$Time,
                                         ".",StateDet)
             
           }
         }     
         else if(Actions[jj]=="E" && as.numeric(StatePosX)!=Parameters$cols)
         {  
           StateVect<-EmptyStateMatrix
           StateVect$ActionHistory<-paste0(StateMatrix[ii,5],".M")
           StateVect$State<-StateMatrix$NextState[ii]
           StateVect$Action<-Actions[jj]
           StateVect$DetainHistory<-StateMatrix[ii,7]
           StateVect$Reward<-Parameters$MoveReward
           StateVect$Time=sprintf("%02d", as.integer(StateMatrix$Time[ii])-Parameters$traveltime)
           newX=sprintf("%02d",as.numeric(StatePosX)+1)
           for(tt in 1:nrow(TownMap))
           {
             if(as.numeric(newX)==TownMap$X[tt]&&
                as.numeric(StatePosY)==TownMap$Y[tt])
             {
               StateDet=TownMap$DetainState[tt]
             }}  
           if (StateVect$DetainHistory=="INI")
           {
             StateVect$NextState<-paste0(newX,".",StatePosY,
                                         ".",StateVect$Time,
                                         ".",StateDet)
           }
           else
           {
             
             StateVect$NextState<-paste0(StateVect$DetainHistory,".",
                                         newX,".",StatePosY,".",StateVect$Time,
                                         ".",StateDet)
           }
         }        
         StateMatrix2=rbind(StateMatrix2,StateVect)
       }
     }
   }
   StateMatrix=rbind(StateMatrix,StateMatrix2)
   StateMatrix=unique(StateMatrix)
   StateMatrix<-StateMatrix[-which(apply(StateMatrix,1,function(x)all(is.na(x)))),]
   rownames(StateMatrix) <- NULL
 }
  return(StateMatrix)
}

## This function compares the suggested policy to the state maatrix to determine the 
## best path, as chosen by the Reinforcment Learning Algorithm.

##Removed for Now

#### The following functions came from the Reinforcement Learning package
#### Github. They either contain modifications for this application or are not
#### contained in the Reinforcment Learning package.

#' Sample state transitions from an environment function
#'
#' Function generates sample experience in the form of state transition tuples.
#'
#' @param N Number of samples.
#' @param env An environment function.
#' @param states A character vector defining the enviroment states.
#' @param actions A character vector defining the available actions.
#' @param actionSelection (optional) Defines the action selection mode of the reinforcement learning agent. Default: \code{random}.
#' @param control (optional) Control parameters defining the behavior of the agent.
#' Default: \code{alpha = 0.1}; \code{gamma = 0.1}; \code{epsilon = 0.1}.
#' @param model (optional) Existing model of class \code{rl}. Default: \code{NULL}.
#' @param ... Additional parameters passed to function.
#' @seealso \code{\link{ReinforcementLearning}}
#' @seealso \code{\link{gridworldEnvironment}}
#' @return An \code{dataframe} containing the experienced state transition tuples \code{s,a,r,s_new}.
#' The individual columns are as follows:
#' \describe{
#'   \item{\code{State}}{The current state.}
#'   \item{\code{Action}}{The selected action for the current state.}
#'   \item{\code{Reward}}{The reward in the current state.}
#'   \item{\code{NextState}}{The next state.}
#' }
#' @examples
#' # Define environment
#' env <- gridworldEnvironment
#'
#' # Define states and actions
#' states <- c("s1", "s2", "s3", "s4")
#' actions <- c("up", "down", "left", "right")
#'
#' # Sample 1000 training examples
#' data <- sampleExperience(N = 1000, env = env, states = states, actions = actions)
#' @export
sampleExperienceX <- function(StateMatrix,N, env, states, actions, actionSelection = "random",
                              control = list(alpha = 0.1, gamma = 0.1, epsilon = 0.1), model = NULL, ...) {
  if (!(N > 0 && length(N) == 1 && is.numeric(N) && floor(N) == N)) {
    stop("Argument 'N' should be an integer > 0.")
  }
  if (!is.function(env)) {
    stop("Argument 'env' describing the environment must be of type function.")
  }
  if(!is.character(states)) {
    stop("Arguments 'states' must be of type 'character'.")
  }
  if(!is.character(actions)) {
    stop("Arguments 'actions' must be of type 'character'.")
  }
  if(class(model) != "rl" && !is.null(model)) {
    stop("Argument 'model' must be empty or of type 'rl'.")
  }
  if (!is.list(control)) {
    stop("Argument 'control' must be of type 'list'.")
  }
  if (is.null(control$epsilon)) {
    stop("Missing or invalid control parameters.")
  }
  if (is.null(model)) {
    Q <- hash()
  } else {
    Q <- model$Q_hash
  }
  for (i in unique(states)[!unique(states) %in% names(Q)]) {
    Q[[i]] <- hash(unique(actions), rep(0, length(unique(actions))))
  }
  
  actionSelectionFunction <- lookupActionSelection(actionSelection)
  
  sampleStates <- sample(states, N, replace = T)
  sampleActions <-
    sapply(sampleStates, function(x)
      actionSelectionFunction(Q, x, control$epsilon))
  
  response <- lapply(1:length(sampleStates),
                     function(x) env(sampleStates[x], sampleActions[x], StateMatrix))
  response <- data.table::rbindlist(lapply(response, data.frame))
  
  out <- data.frame(
    State = sampleStates,
    Action = sampleActions,
    Reward = as.numeric(response$Reward),
    NextState = as.character(response$NextState),
    stringsAsFactors = F
  )
  return(out)
}
#' Performs \eqn{\varepsilon}-greedy action selection
#'
#' Implements \eqn{\varepsilon}-greedy action selection. In this strategy, the agent explores the environment
#' by selecting an action at random with probability \eqn{\varepsilon}. Alternatively, the agent exploits its
#' current knowledge by choosing the optimal action with probability \eqn{1-\varepsilon}.
#'
#' @param Q State-action table of type \code{hash}.
#' @param state The current state.
#' @param epsilon Exploration rate between 0 and 1.
#' @return Character value defining the next action.
#' @import hash
#' @importFrom stats runif
#' @references Sutton and Barto (1998). "Reinforcement Learning: An Introduction", MIT Press, Cambridge, MA.
#' @export
epsilonGreedyActionSelection <- function(Q, state, epsilon) {
  if (runif(1) <= epsilon) {
    best_action <- names(sample(values(Q[[state]]), 1))
  } else {
    best_action <- names(which.max(values(Q[[state]])))
  }
  return(best_action)
}

#' Performs random action selection
#'
#' Performs random action selection. In this strategy, the agent always selects an action at random.
#'
#' @param Q State-action table of type \code{hash}.
#' @param state The current state.
#' @param epsilon Exploration rate between 0 and 1 (not used).
#' @return Character value defining the next action.
#' @import hash
#' @export
randomActionSelection <- function(Q, state, epsilon) {
  return(names(sample(values(Q[[state]]), 1)))
}

#' Converts a name into an action selection function
#'
#' Input is a name for the action selection, output is the corresponding function object.
#'
#' @param type A string denoting the type of action selection. Allowed values are \code{epsilon-greedy} or \code{random}.
#' @return Function that implements the specific learning rule.
lookupActionSelection <- function(type) {
  if (type == "epsilon-greedy") {
    return(epsilonGreedyActionSelection)
  }
  if (type == "random") {
    return(randomActionSelection)
  }
  stop("Rule for action selection not recognized. Corresponding argument has an invalid value.")
}



(* ::Package:: *)

BeginPackage["sac`"]

Simulate::usage = "Generate history of states by simulating numAgents for numIterations with probability p of including each state in the history.";

FirmHistories::usage = "Extract firm histories from history.";

FirmProfitRates::usage = "Extract firm profit rates from firm histories.";

Unemployed::usage = "Get unemployed in state, or over history of states.";

FirmSizes::usage = "Get firm sizes in state, or over history of states.";

EmployeesMoney::usage = "Get workers money holdings in state, or over history of states.";

EmployersWealth::usage = "Get capitalists wealth in state, or over history of states.";

(* Added for testing only *)
Books::usage = "";
GetWages::usage = "";
GetDepreciation::usage="";
GetRevenue::usage = "";
RecordWagePayment::usage = "";
RecordDepreciation::usage = "";
RecordRevenue::usage = "";
Agent::usage = "";
GetMoney::usage = "";
GetEmployer::usage = "";
GetEmployees::usage = "";
GetFixedCapital::usage = "";
GetBooks::usage = "";
SetMoney::usage = "";
SetEmployer::usage = "";
SetEmployees::usage = "";
SetFixedCapital::usage = "";
SetBooks::usage = "";
IsEmployee::usage = "";
IsEmployer::usage = "";
IsUnemployed::usage = "";
InitState::usage = "";
GetAgent::usage = "";
GetUnemployed::usage = "";
ChooseAgent::usage = "";
ChooseAgentWeighted::usage = "";
GetWealth::usage = "";
ChooseAgentWeightedByMoney::usage = "";
ChooseAgentWeightedByPoverty::usage = "";
RandomExponentialReal::usage = "";
Spend::usage = "";
Invest::usage = "";
DeleteFromList::usage = "";
Resign::usage = "";
Hire::usage = "";
FixedCapitalTransfer::usage = "";
Work::usage = "";
Pay::usage = "";
Employees::usage = "";
Employers::usage = "";
MoneyHoldings::usage = "";
WealthHoldings::usage = "";
FixedCapital::usage = "";
EmployersMoney::usage = "";
UnemployedMoney::usage = "";
EmployeesWealth::usage = "";
UnemployedWealth::usage = "";
FirmSizes::usage = "";
FirmHistories::usage = "";
TotalMoneyHoldings::usage = "";
FirmRevenue::usage = "";
FirmDepreciation::usage = "";
FirmWageBill::usage = "";
FirmProfit::usage = "";
FirmProfitRates::usage = "";

Begin["`Private`"]

Books[] := {0.0, 0.0, 0.0}
Books[wages_, depreciation_, revenue_] := {wages, depreciation, revenue}
GetWages[books_] := books[[1]]
GetDepreciation[books_] := books[[2]]
GetRevenue[books_] := books[[3]]
RecordWagePayment[books_, payment_] := {GetWages[books] + payment, GetDepreciation[books], GetRevenue[books]}
RecordDepreciation[books_, depreciation_] := {GetWages[books], GetDepreciation[books] + depreciation, GetRevenue[books]}
RecordRevenue[books_, revenue_] := {GetWages[books], GetDepreciation[books], GetRevenue[books] + revenue}

Agent[money_] := {money, None, {}, 0.0, Books[]}
Agent[money_, employer_, employees_, fixedCapital_, books_] := {money, employer, employees, fixedCapital, books}
GetMoney[agent_] := agent[[1]]
GetEmployer[agent_] := agent[[2]]
GetEmployees[agent_] := agent[[3]]
GetFixedCapital[agent_] := agent[[4]]
GetBooks[agent_] := agent[[5]]
SetMoney[agent_, money_] := Agent[money, GetEmployer[agent], GetEmployees[agent], GetFixedCapital[agent], GetBooks[agent]]
SetEmployer[agent_, employer_] := Agent[GetMoney[agent], employer, GetEmployees[agent], GetFixedCapital[agent], GetBooks[agent]]
SetEmployees[agent_, employees_] := Agent[GetMoney[agent], GetEmployer[agent], employees, GetFixedCapital[agent], GetBooks[agent]]
SetFixedCapital[agent_, fixedCapital_] := Agent[GetMoney[agent], GetEmployer[agent], GetEmployees[agent], fixedCapital, GetBooks[agent]]
SetBooks[agent_, books_] := Agent[GetMoney[agent], GetEmployer[agent], GetEmployees[agent], GetFixedCapital[agent], books]

IsEmployee[agent_] := NumberQ[GetEmployer[agent]]
IsEmployer[agent_] := Length[GetEmployees[agent]] > 0
IsUnemployed[agent_] := !(IsEmployee[agent] || IsEmployer[agent])

InitState[numAgents_, moneyPerAgent_] := Association[{
	"agents" -> Association[With[{m = moneyPerAgent}, Map[# -> Agent[m]&, Range[numAgents]]]], 
	"demand" -> 0
}]

GetAgent[state_, agentId_] := state["agents"][agentId]
GetUnemployed[agents_] := Select[agents, IsUnemployed]
GetWealth[agent_] := GetMoney[agent] + GetFixedCapital[agent]

ChooseAgent[agents_] := RandomChoice[Keys[agents]]
ChooseAgentWeighted[agents_, weights_] := RandomChoice[weights -> Keys[agents]]
ChooseAgentWeightedByMoney[agents_] := ChooseAgentWeighted[agents, (GetMoney /@ Values[agents])]
ChooseAgentWeightedByPoverty[agents_] := Block[{wealth = (GetWealth /@ Values[agents]), maxWealth}, 
	maxWealth = Max[wealth]; 
	ChooseAgentWeighted[agents, Map[1 + maxWealth - #&, wealth]]
]

RandomExponentialReal[m_] := If[m <= 0, m, RandomVariate[TruncatedDistribution[{0, m}, ExponentialDistribution[1 / 2]]]]

Spend[state_, agentId_] := Block[{newState = state, spendingAgent, m, s, td},
  spendingAgent = GetAgent[newState, agentId];
  m = GetMoney[spendingAgent];
  (* Higher probability of spending a small amount of money *)
  (* s = RandomExponentialReal[m]; *)
  s = RandomReal[m];
  newState["demand"] = newState["demand"] + s;
  newState["agents"][agentId] = SetMoney[spendingAgent, m - s];
  newState
]

Spend[state_] := Spend[state, ChooseAgent[state["agents"]]]

Invest[state_, agentId_] := Block[{newState = state, investingAgent, m, s},
  investingAgent = GetAgent[newState, agentId];
  If[IsEmployer[investingAgent],
    m = GetMoney[investingAgent];
    (* Higher probability of investing a small amount of money *)
    (* s = RandomExponentialReal[m]; *)
    s = RandomReal[m];
    investingAgent = SetFixedCapital[investingAgent, GetFixedCapital[investingAgent] + s];
    investingAgent = SetMoney[investingAgent, m - s];
    newState["agents"][agentId] = investingAgent;
    newState["demand"] = newState["demand"] + s;
  ];
  newState
]

Invest[state_] := Invest[state, ChooseAgent[state["agents"]]]

DeleteFromList[list_, value_] := Block[{position = FirstPosition[list, value]}, 
	If[MissingQ[position], list, Drop[list, position]]
]

Resign[agents_, resigningAgentId_] := Block[{newAgents = agents, employerId, resigningAgent}, 
	If[IsEmployee[agents[resigningAgentId]], 
		employerId = GetEmployer[agents[resigningAgentId]];
		newAgents[employerId] = SetEmployees[newAgents[employerId], DeleteFromList[GetEmployees[newAgents[employerId]], resigningAgentId]];
		resigningAgent = newAgents[resigningAgentId];
		resigningAgent = SetEmployer[resigningAgent, None];
		newAgents[resigningAgentId] = resigningAgent;
	]; 
	newAgents
]

Hire[agents_, hiringAgentId_, hireeAgentId_] := Block[{newAgents = agents, employer, employees},
  (* Hiree agent resigns *)
  newAgents = Resign[newAgents, hireeAgentId];
  (* Hiring agent gets a new employee *)
  newAgents[hiringAgentId] = SetEmployees[newAgents[hiringAgentId], Join[GetEmployees[newAgents[hiringAgentId]], {hireeAgentId}]];
  (* Hiree agent gets a new employer. *)
  newAgents[hireeAgentId] = SetEmployer[newAgents[hireeAgentId], hiringAgentId];
  newAgents
]

(* TODO: more employers means less chance to hire ... which may be wrong. *)
Hire[agents_] := Block[{hiringAgentId, hireeAgentId},
	hiringAgentId = ChooseAgentWeightedByMoney[agents];
	hireeAgentId = ChooseAgentWeightedByPoverty[agents];
	(* Both employed and unemployed can get hired. *)
	If[hiringAgentId != hireeAgentId && !IsEmployee[agents[hiringAgentId]] && !IsEmployer[agents[hireeAgentId]],
		Hire[agents, hiringAgentId, hireeAgentId], 
		agents
	]
]

FixedCapitalTransfer[agents_, workingAgentId_] := Block[{newAgents = agents, employerId, fixedCapitalTransfer = 0.0, numFirmEmployees, depreciationRate = 0.02},
	If[IsEmployee[agents[workingAgentId]],
		employerId = GetEmployer[agents[workingAgentId]];
		(* Each employee transfers a proportion of the firm's fixed capital *)
		fixedCapitalTransfer = depreciationRate * GetFixedCapital[agents[employerId]] / Length[GetEmployees[agents[employerId]]];
		newAgents[employerId] = SetFixedCapital[newAgents[employerId], GetFixedCapital[newAgents[employerId]] - fixedCapitalTransfer];
	]; 
	{newAgents, fixedCapitalTransfer}
]

Work[state_, agentId_] := Block[{newState, workingAgentId, valueAdded, employerAgentId, employerAgent, fixedCapitalTransfer = 0.0},
  If[!IsUnemployed[GetAgent[state, agentId]],
    newState = state;
    {newState["agents"], fixedCapitalTransfer} = FixedCapitalTransfer[newState["agents"], agentId];
    (* Transfer value of fixed capital and sample some value-add from effective demand *)
    valueAdded = Min[newState["demand"], fixedCapitalTransfer + RandomReal[newState["demand"]]];
    newState["demand"] = newState["demand"] - valueAdded;
    (* Transfer to employer *)
    With[{workingAgent = GetAgent[state, agentId]},
       employerAgentId = If[IsEmployee[workingAgent], GetEmployer[workingAgent], agentId];
    ];
    employerAgent = newState["agents"][employerAgentId];
    newState["agents"][employerAgentId] = SetMoney[employerAgent, GetMoney[employerAgent] + valueAdded];
    (* Record the depreciation *)
    employerAgent = newState["agents"][employerAgentId];
    newState["agents"][employerAgentId] = SetBooks[employerAgent, RecordDepreciation[GetBooks[employerAgent], fixedCapitalTransfer]];
    (* Record the revenue *)
    employerAgent = newState["agents"][employerAgentId];
    newState["agents"][employerAgentId] = SetBooks[employerAgent, RecordRevenue[GetBooks[employerAgent], valueAdded]];
    newState,
    (* else no work done *)
    state
  ]
]

Work[state_] := Work[state, ChooseAgent[state["agents"]]]

Pay[agents_, employeeId_] := Block[{newAgents = agents, employerIds, employerId, employeeIds, wage},
  If[IsEmployee[newAgents[employeeId]],
    employerId = GetEmployer[newAgents[employeeId]];
    If[employerId != employeeId,
      (* Wage is crucial parameter. Too low and 1 firm dominates. Too high and no large firms form. Also controls unemployment rate *)
      wage = RandomReal[1.0];
      If[GetMoney[newAgents[employerId]] >= wage,
        newAgents[employerId] = SetMoney[newAgents[employerId], GetMoney[newAgents[employerId]] - wage];
        newAgents[employeeId] = SetMoney[newAgents[employeeId], GetMoney[newAgents[employeeId]] + wage];
        (* Update books *)
        newAgents[employerId] = SetBooks[newAgents[employerId], RecordWagePayment[GetBooks[newAgents[employerId]], wage]];,
        (* else fire employee if run out of money (and don't pay them) *)
        newAgents = Resign[newAgents, employeeId];
        (* If this is last employee, and firm dissolves, then close the books *)
        (* TODO: may want to scrap fixed-capital too at this point *)
        If[!IsEmployer[newAgents[employerId]],
           newAgents[employerId] = SetBooks[newAgents[employerId], Books[]];
        ];
      ];
    ];
  ];
  newAgents
]

Pay[agents_] := Pay[agents, ChooseAgent[agents]]

Simulate[numAgents_, numIterations_, p_] := Module[{state = InitState[numAgents, 1], i = numIterations, rules},
  rules = {
    Hold[state = Work[state]],(* random agent *)
    Hold[state["agents"] = Pay[state["agents"]]],(* random agent *)
    Hold[state = Invest[state]],(* random agent *)
    Hold[state = Spend[state]],(* random agent *)
    Hold[state["agents"] = Hire[state["agents"]]](* weighted by money: vital for large firm formation *)
  };
  First[Last[Reap[
     Monitor[Do[
           i--;
           ReleaseHold /@ RandomSample[rules];
           If[RandomReal[] <= p, Sow[state]],
           numIterations
     ], i]
  ]]]
]

(* Returns association of employees *)
Employees[state_] := Select[state["agents"], IsEmployee]

(* Returns association of employers *)
Employers[state_] := Select[state["agents"], IsEmployer]

(* Returns association of unemployed *)
Unemployed[state_] := Select[state["agents"], IsUnemployed]

(* Returns money holdings for every agent *)
MoneyHoldings[agents_] := Map[GetMoney[#]&, Values[agents]]

(* Returns wealth holdings for every agent *)
WealthHoldings[agents_] := Map[GetMoney[#] + GetFixedCapital[#]&, Values[agents]]

(* Returns fixed capital held by every agent *)
FixedCapital[agents_] := Map[GetFixedCapital[#]&, Values[agents]]

(* Returns money holdings for every employee *)
EmployeesMoney[state_] := MoneyHoldings[Employees[state]]

(* Returns money holdings for every employer *)
EmployersMoney[state_] := MoneyHoldings[Employers[state]]

(* Returns money holdings for every unemployed *)
UnemployedMoney[state_] := MoneyHoldings[Unemployed[state]]

(* Returns wealth holdings for every employee *)
EmployeesWealth[state_] := WealthHoldings[Employees[state]]

(* Returns wealth holdings for every employer *)
EmployersWealth[state_] := WealthHoldings[Employers[state]]

(* Returns wealth holdings for every unemployed *)
UnemployedWealth[state_] := WealthHoldings[Unemployed[state]]

(* Returns list of firm sizes *)
FirmSizes[state_] := Length /@ GetEmployees/@ Values[Employers[state]]

(* Returns a list of firm histories, where a firm history is a time-ordered list of states of the agent when they were an employer *)
(* N.B. if p!=1 then multiple firm lifetimes can be collapsed to 1 firm lifetime. In which case, Books may be reset during apparent lifetime of the firm. *)
FirmHistories[history_List] := Module[{firmHistories},
   PrintTemporary["Construct histories"];
   (* Construct a list of associations of firms in each simulation step *)
   firmHistories = Flatten[ResourceFunction["DynamicMap"][DeleteCases[Map[If[IsEmployer[#], #, {}] &, #["agents"]], {}] &, history]];
   PrintTemporary["Merge histories"];
   firmHistories = Merge[firmHistories, Join];
   (* Split history lists into sequences when the agent was an employer and when it was not *)
   PrintTemporary["Split histories"];
   firmHistories = ResourceFunction["DynamicMap"][SplitBy[#, Length[#] == 0 &] &, firmHistories];
   PrintTemporary["Remove non-firm data"];
   (* Delete sequences when the agent was not an employer, leaving a sequence of firm lifetimes (consisting of a sequence of agent states) for each agent *)
   firmHistories = ResourceFunction["DynamicMap"][Map[If[Length[DeleteCases[#, {}]] == 0, Nothing, #] &, #]&, firmHistories];
   Flatten[Values[firmHistories], 1]
]

(* Returns a list of the wealth of all employers at all times *)
EmployersWealth[history_List] := Flatten[ResourceFunction["DynamicMap"][EmployersWealth, history]]

(* Returns a list of the wealth of all employees at all times *)
EmployeesWealth[history_List] := Flatten[ResourceFunction["DynamicMap"][EmployeesWealth, history]]

(* Returns a list of the wealth of all unemployed at all times *)
UnemployedWealth[history_List] := Flatten[ResourceFunction["DynamicMap"][UnemployedWealth, history]]

(* Returns a list of the money holdings of all employees at all times *)
EmployeesMoney[history_List] := Flatten[ResourceFunction["DynamicMap"][EmployeesMoney, history]]

(* Returns a list of the firm sizes of all firms at all times *)
FirmSizes[history_List] := Flatten[ResourceFunction["DynamicMap"][FirmSizes, history]]

(* Returns a list of number of unemployed at all times *)
Unemployed[history_List] := Length /@ Flatten[ResourceFunction["DynamicMap"][Unemployed, history]]

(* Returns a list of the total aggregate money held by agents at all times *)
TotalMoneyHoldings[history_List] := Total /@ ResourceFunction["DynamicMap"][MoneyHoldings[#["agents"]]&, history]

(* Returns list of total lifetime revenue of all firms that existed *)
FirmRevenue[firmHistory_List] := GetRevenue /@ GetBooks /@ Last /@ firmHistory

(* Returns list of total lifetime depreciation of all firms that existed *)
FirmDepreciation[firmHistory_List] := GetDepreciation /@ GetBooks /@ Last /@ firmHistory

(* Returns list of total lifetime wage bill of all firms that existed *)
FirmWageBill[firmHistory_List] := GetWages /@ GetBooks /@ Last /@ firmHistory

FirmProfit[firmHistory_List] := Module[
   {
      firmRevenue = FirmRevenue[firmHistory], 
      firmDepreciation = FirmDepreciation[firmHistory],
      firmWageBill = FirmWageBill[firmHistory],
      costs
   }, 
   costs = firmWageBill + firmDepreciation;
   firmRevenue - costs
]

FirmProfitRates[firmHistory_List] := Module[
   {
      firmRevenue = FirmRevenue[firmHistory],
      firmDepreciation = FirmDepreciation[firmHistory], 
      firmWageBill = FirmWageBill[firmHistory], 
      costs
   },
   costs = firmWageBill + firmDepreciation;
   Quiet[(firmRevenue - costs) / costs]
]



End[]

EndPackage[]

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
ExtendAgent::usage = "";
IntendAgent::usage = "";
ExtendState::usage = "";
IntendState::usage = "";
BooksName::usage = "";
ExtendBooks::usage = "";

Begin["`Private`"]

Books[name_] := Module[{wages = 0.0, depreciation = 0.0, revenue = 0.0},
   name[GetWages] := wages;
   name[SetWages[x_]] := wages = x;
   name[GetDepreciation] := depreciation;
   name[SetDepreciation[x_]] := depreciation = x;
   name[GetRevenue] := revenue;
   name[SetRevenue[x_]] := revenue = x;
   name
]

BooksName[name_] := Books[name] = Symbol["books" <> ToString[name]]

ExtendBooks[name_] := {name[GetWages], name[GetDepreciation], name[GetRevenue]}

IntendBooks[name_, object_] := Block[{},
   (*If[!NameQ[name, ResolveContextAliases \[Rule] True], Books[name]];*)
   Books[name];
   name[SetWages[object[[1]]]];
   name[SetDepreciation[object[[2]]]];
   name[SetRevenue[object[[3]]]];
   name
]

RecordWagePayment[books_, payment_] := (books[SetWages[books[GetWages] + payment]]; books)
RecordDepreciation[books_, depreciation_] := (books[SetDepreciation[books[GetDepreciation] + depreciation]]; books)
RecordRevenue[books_, revenue_] := (books[SetRevenue[books[GetRevenue] + revenue]]; books)

Agent[name_] := Module[{money = 0.0, employer = None, employees = {}, fixedCapital = 0.0, books = Books[BooksName[name]]},
   name[GetMoney] := money;
   name[SetMoney[x_]] := money = x;
   name[GetEmployer] := employer;
   name[SetEmployer[x_]] := employer = x;
   name[GetEmployees] := employees;
   name[SetEmployees[x_]] := employees = x;
   name[GetFixedCapital] := fixedCapital;
   name[SetFixedCapital[x_]] := fixedCapital = x;
   name[GetBooks] := books;
   name[SetBooks[x_]] := books = x;
   name[GetWealth] := money + fixedCapital;
   name
]

AgentName[id_] := AgentName[id] = Symbol["agent" <> ToString[id]]

ExtendAgent[name_] := {name[GetMoney], name[GetEmployer], name[GetEmployees], name[GetFixedCapital], ExtendBooks[name[GetBooks]]}

IntendAgent[name_, object_] := Block[{},
   (* If[!NameQ[name, ResolveContextAliases \[Rule] True], Agent[name]]; *)
   Agent[name];
   name[SetMoney[object[[1]]]];
   name[SetEmployer[object[[2]]]];
   name[SetEmployees[object[[3]]]];
   name[SetFixedCapital[object[[4]]]];
   name[SetBooks[IntendBooks[BooksName[name], object[[5]]]]];
   name
]
IntendAgent[object_] := IntendAgent[agent0, object]

IsEmployee[agent_] := NumberQ[agent[GetEmployer]]
IsEmployer[agent_] := Length[agent[GetEmployees]] > 0
IsUnemployed[agent_] := !(IsEmployee[agent] || IsEmployer[agent])

InitState[numAgents_, moneyPerAgent_] := Association[{
	"agents" -> Association[With[{m = moneyPerAgent},
	   Map[With[{agent = AgentName[#]},
	         Agent[agent];
	         agent[SetMoney[m]];
	         # -> agent
	      ] &,
	      Range[numAgents]]
	   ]
	], 
	"demand" -> 0
}]

GetAgent[state_, agentId_] := state["agents"][agentId]
GetUnemployed[agents_] := Select[agents, IsUnemployed]

ChooseAgent[agents_] := RandomChoice[Keys[agents]]
ChooseAgentWeighted[agents_, weights_] := RandomChoice[weights -> Keys[agents]]

ChooseAgentWeightedByMoney[agents_] := ChooseAgentWeighted[agents, #[GetMoney]& /@ Values[agents]]
ChooseAgentWeightedByPoverty[agents_] := Block[{wealth = #[GetWealth]& /@ Values[agents], maxWealth}, 
	maxWealth = Max[wealth]; 
	ChooseAgentWeighted[agents, Map[1 + maxWealth - #&, wealth]]
]

RandomExponentialReal[m_] := If[m <= 0, m, RandomVariate[TruncatedDistribution[{0, m}, ExponentialDistribution[1 / 2]]]]

Spend[state_, agentId_] := Block[{newState = state, spendingAgent, m, s, td},
  spendingAgent = GetAgent[newState, agentId];
  m = spendingAgent[GetMoney];
  (* Higher probability of spending a small amount of money *)
  (* s = RandomExponentialReal[m]; *)
  s = RandomReal[m];
  newState["demand"] = newState["demand"] + s;
  spendingAgent[SetMoney[m - s]];
  newState
]

Spend[state_] := Spend[state, ChooseAgent[state["agents"]]]

Invest[state_, agentId_] := Block[{newState = state, investingAgent, m, s},
  investingAgent = GetAgent[newState, agentId];
  If[IsEmployer[investingAgent],
    m = investingAgent[GetMoney];
    (* Higher probability of investing a small amount of money *)
    (* s = RandomExponentialReal[m]; *)
    s = RandomReal[m];
    investingAgent[SetFixedCapital[investingAgent[GetFixedCapital] + s]];
    investingAgent[SetMoney[m - s]];
    newState["demand"] = newState["demand"] + s;
  ];
  newState
]

Invest[state_] := Invest[state, ChooseAgent[state["agents"]]]

DeleteFromList[list_, value_] := Block[{position = FirstPosition[list, value]}, 
	If[MissingQ[position], list, Drop[list, position]]
]

Resign[agents_, resigningAgentId_] := Block[{employerId, resigningAgent}, 
	If[IsEmployee[agents[resigningAgentId]],
	    employerId = agents[resigningAgentId][GetEmployer];
		agents[employerId][SetEmployees[DeleteFromList[agents[employerId][GetEmployees], resigningAgentId]]];
		agents[resigningAgentId][SetEmployer[None]];
	]; 
	agents
]

Hire[agents_, hiringAgentId_, hireeAgentId_] := Block[{employer, employees},
  (* Hiree agent resigns *)
  Resign[agents, hireeAgentId];
  (* Hiring agent gets a new employee *)
  agents[hiringAgentId][SetEmployees[Join[agents[hiringAgentId][GetEmployees], {hireeAgentId}]]];
  (* Hiree agent gets a new employer. *)
  agents[hireeAgentId][SetEmployer[hiringAgentId]];
  agents
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

FixedCapitalTransfer[agents_, workingAgentId_] := Block[{employerId, fixedCapitalTransfer = 0.0, numFirmEmployees, depreciationRate = 0.02},
	If[IsEmployee[agents[workingAgentId]],
		employerId = agents[workingAgentId][GetEmployer];
		(* Each employee transfers a proportion of the firm's fixed capital *)
		fixedCapitalTransfer = depreciationRate * agents[employerId][GetFixedCapital] / Length[agents[employerId][GetEmployees]];
		agents[employerId][SetFixedCapital[agents[employerId][GetFixedCapital] - fixedCapitalTransfer]];
	]; 
	fixedCapitalTransfer
]

Work[state_, agentId_] := Block[{newState, workingAgentId, valueAdded, employerAgentId, employerAgent, fixedCapitalTransfer = 0.0},
  If[!IsUnemployed[GetAgent[state, agentId]],
    newState = state;
    fixedCapitalTransfer = FixedCapitalTransfer[newState["agents"], agentId];
    (* Transfer value of fixed capital and sample some value-add from effective demand *)
    valueAdded = Min[newState["demand"], fixedCapitalTransfer + RandomReal[newState["demand"]]];
    newState["demand"] = newState["demand"] - valueAdded;
    (* Transfer to employer *)
    With[{workingAgent = GetAgent[state, agentId]},
       employerAgentId = If[IsEmployee[workingAgent], workingAgent[GetEmployer], agentId];
    ];
    employerAgent = newState["agents"][employerAgentId];
    employerAgent[SetMoney[employerAgent[GetMoney] + valueAdded]];
    (* Record the depreciation *)
    RecordDepreciation[BooksName[employerAgent], fixedCapitalTransfer];
    (* employerAgent[SetBooks[RecordDepreciation[employerAgent[GetBooks], fixedCapitalTransfer]]]; *)
    (* Record the revenue *)
    RecordRevenue[BooksName[employerAgent], valueAdded];
    (*employerAgent[SetBooks[RecordRevenue[employerAgent[GetBooks], valueAdded]]];*)
    newState,
    (* else no work done *)
    state
  ]
]

Work[state_] := Work[state, ChooseAgent[state["agents"]]]

Pay[agents_, employeeId_] := Block[{employerIds, employerId, employeeIds, wage},
  If[IsEmployee[agents[employeeId]],
    employerId = agents[employeeId][GetEmployer];
    If[employerId != employeeId,
      (* Wage is crucial parameter. Too low and 1 firm dominates. Too high and no large firms form. Also controls unemployment rate *)
      wage = RandomReal[1.0];
      If[agents[employerId][GetMoney] >= wage,
        agents[employerId][SetMoney[agents[employerId][GetMoney] - wage]];
        agents[employeeId][SetMoney[agents[employeeId][GetMoney] + wage]];
        (* Update books *)
        RecordWagePayment[agents[employerId][GetBooks], wage];
        (* agents[employerId][SetBooks[RecordWagePayment[agents[employerId][GetBooks], wage]]]; *)
        (* else fire employee if run out of money (and don't pay them) *)
        Resign[agents, employeeId];
        (* If this is last employee, and firm dissolves, then close the books *)
        (* TODO: may want to scrap fixed-capital too at this point *)
        If[!IsEmployer[agents[employerId]],
           Books[agents[employerId][GetBooks]];
           (*agents[employerId][SetBooks[Books[]]];*)
        ];
      ];
    ];
  ];
  agents
]

Pay[agents_] := Pay[agents, ChooseAgent[agents]]

ExtendState[intendedState_] := MapAt[Map[ExtendAgent[#] &, #] &, intendedState, Key["agents"]]
IntendState[extendedState_] := MapAt[Association[KeyValueMap[With[{key = #1, value = #2}, key -> IntendAgent[AgentName[key], value]] &, #]] &, extendedState, Key["agents"]]

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
           If[RandomReal[] <= p, Sow[ExtendState[state]]],
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
MoneyHoldings[agents_] := Map[#[GetMoney]&, Values[agents]]

(* Returns wealth holdings for every agent *)
WealthHoldings[agents_] := Map[#[GetWealth]&, Values[agents]]

(* Returns fixed capital held by every agent *)
FixedCapital[agents_] := Map[#[GetFixedCapital]&, Values[agents]]

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
FirmSizes[state_] := Length /@ (#[GetEmployees]& /@ Values[Employers[state]])

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
EmployersWealth[history_List] := Flatten[ResourceFunction["DynamicMap"][EmployersWealth[IntendState[#]]&, history]]

(* Returns a list of the wealth of all employees at all times *)
EmployeesWealth[history_List] := Flatten[ResourceFunction["DynamicMap"][EmployeesWealth[IntendState[#]]&, history]]

(* Returns a list of the wealth of all unemployed at all times *)
UnemployedWealth[history_List] := Flatten[ResourceFunction["DynamicMap"][UnemployedWealth[IntendState[#]]&, history]]

(* Returns a list of the money holdings of all employees at all times *)
EmployeesMoney[history_List] := Flatten[ResourceFunction["DynamicMap"][EmployeesMoney[IntendState[#]]&, history]]

(* Returns a list of the firm sizes of all firms at all times *)
FirmSizes[history_List] := Flatten[ResourceFunction["DynamicMap"][FirmSizes[IntendState[#]]&, history]]

(* Returns a list of number of unemployed at all times *)
Unemployed[history_List] := Length /@ Flatten[ResourceFunction["DynamicMap"][Unemployed[IntendState[#]]&, history]]

(* Returns a list of the total aggregate money held by agents at all times *)
TotalMoneyHoldings[history_List] := Total /@ ResourceFunction["DynamicMap"][MoneyHoldings[IntendState[#]["agents"]]&, history]

(* Returns list of total lifetime revenue of all firms that existed *)
FirmRevenue[firmHistory_List] := IntendAgent[#][GetBooks][GetRevenue] & /@ Last /@ firmHistory

(* Returns list of total lifetime depreciation of all firms that existed *)
FirmDepreciation[firmHistory_List] := IntendAgent[#][GetBooks][GetDepreciation] & /@ Last /@ firmHistory

(* Returns list of total lifetime wage bill of all firms that existed *)
FirmWageBill[firmHistory_List] := IntendAgent[#][GetBooks][GetWages] & /@ Last /@ firmHistory

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

(* ::Package:: *)

BeginPackage["sac`"]

Simulate::usage = "Generate history of states by simulating numAgents for numIterations.";

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
ExtendState::usage = "";
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

ExtendBooks[name_] := {name[GetWages], name[GetDepreciation], name[GetRevenue]}
GetWages[books_List] := books[[1]]
GetDepreciation[books_List] := books[[2]]
GetRevenue[books_List] := books[[3]]

BooksName[name_] := Books[name] = Symbol["books" <> ToString[name]]

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

ExtendAgent[name_] := {name[GetMoney], name[GetEmployer], name[GetEmployees], name[GetFixedCapital], ExtendBooks[name[GetBooks]]}
GetMoney[agent_List] := agent[[1]]
GetEmployer[agent_List] := agent[[2]]
GetEmployees[agent_List] := agent[[3]]
GetFixedCapital[agent_List] := agent[[4]]
GetBooks[agent_List] := agent[[5]]
GetWealth[agent_List] := GetMoney[agent] + GetFixedCapital[agent]

AgentName[id_] := AgentName[id] = Symbol["agent" <> ToString[id]]

EmployeeTest[None] := False
EmployeeTest[agent_] := True
IsEmployee[agent_] := EmployeeTest[agent[GetEmployer]]
IsEmployee[agent_List] := EmployeeTest[GetEmployer[agent]]

IsEmployer[agent_] := Length[agent[GetEmployees]] > 0
IsEmployer[agent_List] := Length[GetEmployees[agent]] > 0

IsUnemployed[agent_] := !(IsEmployee[agent] || IsEmployer[agent])

InitState[numAgents_, moneyPerAgent_] := Association[{
	"agents" -> Map[With[{agent = AgentName[#]},
	   Agent[agent];
	   agent[SetMoney[moneyPerAgent]];
	   agent] &,
	   Range[numAgents]
	], 
	"demand" -> 0
}]

GetUnemployed[agents_] := Select[agents, IsUnemployed]

ChooseAgent[agents_] := RandomChoice[agents]
ChooseAgentWeighted[agents_, weights_] := RandomChoice[weights -> agents]

ChooseAgentWeightedByMoney[agents_] := ChooseAgentWeighted[agents, #[GetMoney]& /@ agents]
ChooseAgentWeightedByPoverty[agents_] := Block[{money = #[GetMoney]& /@ agents, maxMoney}, 
	maxMoney = Max[money]; 
	ChooseAgentWeighted[agents, Map[1 + maxMoney - #&, money]]
]

RandomExponentialReal[m_] := If[m <= 0, m, RandomVariate[TruncatedDistribution[{0, m}, ExponentialDistribution[1 / 2]]]]

Spend[state_, agent_] := Block[{newState = state, m, s},
  m = agent[GetMoney];
  (* Higher probability of spending a small amount of money *)
  (* s = RandomExponentialReal[m]; *)
  s = RandomReal[m];
  newState["demand"] = newState["demand"] + s;
  agent[SetMoney[m - s]];
  {newState, s}
]

Spend[state_] := First[Spend[state, ChooseAgent[state["agents"]]]]

(* Self-employed cannot invest *)
Invest[state_, agent_] := Block[{s, newState},
  If[IsEmployer[agent],
    {newState, s} = Spend[state, agent];
    agent[SetFixedCapital[agent[GetFixedCapital] + s]];
    newState,
    state
  ]
]

Invest[state_] := Invest[state, ChooseAgent[state["agents"]]]

DeleteFromList[list_, value_] := Block[{position = FirstPosition[list, value]}, 
	If[MissingQ[position], list, Drop[list, position]]
]

Resign[agent_] := Block[{employer}, 
	If[IsEmployee[agent],
	    employer = agent[GetEmployer];
		employer[SetEmployees[DeleteFromList[employer[GetEmployees], agent]]];
		agent[SetEmployer[None]];
		If[!IsEmployer[employer],
           (* Firm dissolved. Close the books. *)
           Books[employer[GetBooks]];
           (* Scrap fixed-capital *)
           employer[SetFixedCapital[0.0]];
        ];
	]; 
]

Hire[hiringAgent_, hireeAgent_] := Block[{},
  (* Hiree agent resigns *)
  Resign[hireeAgent];
  (* Hiring agent gets a new employee *)
  hiringAgent[SetEmployees[Join[hiringAgent[GetEmployees], {hireeAgent}]]];
  (* Hiree agent gets a new employer. *)
  hireeAgent[SetEmployer[hiringAgent]];
]

Hire[agents_] := Block[{hiringAgent, hireeAgent},
	hiringAgent = ChooseAgentWeightedByMoney[agents];
	hireeAgent = ChooseAgentWeightedByPoverty[agents];
	(* Employee cannot hire. Employer cannot be hired. Employed cannot be hired. *)
	If[!(hiringAgent === hireeAgent) && !IsEmployee[hiringAgent] && IsUnemployed[hireeAgent],
		Hire[hiringAgent, hireeAgent] 
	];
]

(* Fixed capital can transfer even for self-employed *)
FixedCapitalTransfer[agent_] := Block[{employer, fixedCapitalTransfer = 0.0, numFirmEmployees, depreciationRate = 0.05},
    employer = If[IsEmployee[agent], agent[GetEmployer], agent];
	(* Each employee transfers a proportion of the firm's fixed capital *)
	fixedCapitalTransfer = depreciationRate * employer[GetFixedCapital] / (Length[employer[GetEmployees]] + 1);
	employer[SetFixedCapital[employer[GetFixedCapital] - fixedCapitalTransfer]];
	fixedCapitalTransfer
]

Work[state_, agent_] := Block[{newState, valueAdded, employer, fixedCapitalTransfer = 0.0},
    If[!IsUnemployed[agent],
       newState = state;
       fixedCapitalTransfer = FixedCapitalTransfer[agent];
       (* Transfer value of fixed capital and sample some value-add from effective demand *)
       valueAdded = Min[newState["demand"], fixedCapitalTransfer + RandomReal[newState["demand"]]];
       newState["demand"] = newState["demand"] - valueAdded;
       (* Transfer to employer *)
       employer = If[IsEmployee[agent], agent[GetEmployer], agent];
       employer[SetMoney[employer[GetMoney] + valueAdded]];
       (* Record the depreciation *)
       RecordDepreciation[BooksName[employer], fixedCapitalTransfer];
       (* Record the revenue *)
       RecordRevenue[BooksName[employer], valueAdded];
       newState,
       state
    ]
]

Work[state_] := Work[state, ChooseAgent[state["agents"]]]

Pay[employee_] := Block[{employer, wage},
  If[IsEmployee[employee],
    employer = employee[GetEmployer];
    If[!(employer === employee),
      (* Wage is crucial parameter. Too low and 1 firm dominates. Too high and no large firms form. Also controls unemployment rate *)
      wage = RandomReal[{0.1, 0.9}];
      If[employer[GetMoney] >= wage,
        employer[SetMoney[employer[GetMoney] - wage]];
        employee[SetMoney[employee[GetMoney] + wage]];
        (* Update books *)
        RecordWagePayment[employer[GetBooks], wage];,
        (* else fire employee if run out of money (and don't pay them) *)
        Resign[employee];
      ];
    ];
  ];
]

Pay[agent_] := Block[{},
  If[IsEmployer[agent],
     Scan[Block[{employee = #, wage = RandomReal[1.0]},
        If[agent[GetMoney] >= wage,
           agent[SetMoney[agent[GetMoney] - wage]];
           employee[SetMoney[employee[GetMoney] + wage]];
           (* Update books *)
           RecordWagePayment[agent[GetBooks], wage];,
           (* else fire employee if run out of money (and don't pay them) *)
           Resign[employee];
        ]] &, 
        agent[GetEmployees]
     ];
  ];
]

Pay[agents_List] := Pay[ChooseAgent[agents]];

ExtendState[intendedState_] := MapAt[Association[MapIndexed[#2[[1]] -> ExtendAgent[#1] &, #]] &, intendedState, Key["agents"]]

Simulate[numAgents_, numIterations_] := Module[{state = InitState[numAgents, 1], i = numIterations, rules},
  rules = {
    Hold[state = Work[state]],(* random agent *)
    Hold[Pay[state["agents"]]],(* random agent *)
    Hold[state = Invest[state]], (* random agent *)
    Hold[state = Spend[state]],(* random agent *)
    Hold[Hire[state["agents"]]](* weighted by money: vital for large firm formation *)
  };
  First[Last[Reap[
     Monitor[Do[
           i--;
           ReleaseHold /@ RandomSample[rules];
           Sow[ExtendState[state]];,
           numIterations
     ], numIterations + 1 - i]
  ]]]
]

(* Data analysis *)

(* Returns association of employees *)
Employees[state_] := Select[Values[state["agents"]], IsEmployee]

(* Returns association of employers *)
Employers[state_] := Select[Values[state["agents"]], IsEmployer]

(* Returns association of unemployed *)
Unemployed[state_] := Select[Values[state["agents"]], IsUnemployed]

(* Returns money holdings for every agent *)
MoneyHoldings[agents_] := GetMoney /@ agents

(* Returns wealth holdings for every agent *)
WealthHoldings[agents_] := GetWealth /@ agents

(* Returns fixed capital held by every agent *)
FixedCapital[agents_] := GetFixedCapital /@ agents

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
FirmSizes[state_] := Length /@ GetEmployees /@ Employers[state]

(* Returns a list of firm histories, where a firm history is a time-ordered list of states of the agent when they were an employer *)
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
EmployersWealth[history_List] := Flatten[ResourceFunction["DynamicMap"][EmployersWealth[#]&, history]]

(* Returns a list of the wealth of all employees at all times *)
EmployeesWealth[history_List] := Flatten[ResourceFunction["DynamicMap"][EmployeesWealth[#]&, history]]

(* Returns a list of the wealth of all unemployed at all times *)
UnemployedWealth[history_List] := Flatten[ResourceFunction["DynamicMap"][UnemployedWealth[#]&, history]]

(* Returns a list of the money holdings of all employees at all times *)
EmployeesMoney[history_List] := Flatten[ResourceFunction["DynamicMap"][EmployeesMoney[#]&, history]]

(* Returns a list of the firm sizes of all firms at all times *)
FirmSizes[history_List] := Flatten[ResourceFunction["DynamicMap"][FirmSizes[#]&, history]]

(* Returns a list of number of unemployed at all times *)
Unemployed[history_List] := Flatten[ResourceFunction["DynamicMap"][Length[Unemployed[#]]&, history]]

(* Returns a list of the total aggregate money held by agents at all times *)
TotalMoneyHoldings[history_List] := ResourceFunction["DynamicMap"][Total[MoneyHoldings[#["agents"]]]&, history]

(* Returns list of total lifetime revenue of all firms that existed *)
FirmRevenue[firmHistory_List] := GetRevenue[GetBooks[#]] & /@ Last /@ firmHistory

(* Returns list of total lifetime depreciation of all firms that existed *)
FirmDepreciation[firmHistory_List] := GetDepreciation[GetBooks[#]] & /@ Last /@ firmHistory

(* Returns list of total lifetime wage bill of all firms that existed *)
FirmWageBill[firmHistory_List] := GetWages[GetBooks[#]] & /@ Last /@ firmHistory

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

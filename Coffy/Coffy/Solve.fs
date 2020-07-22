namespace Coffy

open Coffy.Types


module internal ORTools =

    open Google.OrTools.LinearSolver


    let private buildExpression (varMap:Map<DecisionName,Variable>) (expr:LinearExpression) =
        let decisionExpr =
            expr.Names
            |> Seq.map (fun n -> expr.Coefficients.[n] * varMap.[n])
            |> fun x -> 
                match Seq.isEmpty x with 
                | true -> LinearExpr() 
                | false -> Seq.reduce (+) x
        
        expr.Offset + decisionExpr


    let private createVariable (solver:Solver) (DecisionName name:DecisionName) (decisionType:DecisionType) =
        match decisionType with
        | Boolean -> solver.MakeBoolVar(name)
        | Integer (lb, ub) -> solver.MakeIntVar(float lb, float ub, name)


    let private createVariableMap (solver:Solver) (decisionMap:Map<DecisionName, Decision>) =
        decisionMap
        |> Map.map (fun n d -> createVariable solver n d.Type)


    let private setObjective (varMap:Map<DecisionName, Variable>) (objective:Coffy.Types.Objective) (solver:Solver) =
        let expr = buildExpression varMap objective.Expression

        match objective.Sense with
        | Minimize -> solver.Minimize(expr)
        | Maximize -> solver.Maximize(expr)


    let private addEqualityConstraint (varMap:Map<DecisionName, Variable>) (ConstraintName n:ConstraintName) (lhs:LinearExpression) (rhs:LinearExpression) (solver:Solver) =
        let lhsExpr = buildExpression varMap lhs
        let rhsExpr = buildExpression varMap rhs
        let c = Google.OrTools.LinearSolver.Equality(lhsExpr, rhsExpr, true)
        solver.Add(c)


    let private addInequalityConstraint (varMap:Map<DecisionName, Variable>) (ConstraintName n:ConstraintName) (lhs:LinearExpression) (rhs:LinearExpression) (inequality:Inequality) (solver:Solver) =
        let lhsExpr = buildExpression varMap lhs
        let rhsExpr = buildExpression varMap rhs
        let constraintExpr = lhsExpr - rhsExpr

        match inequality with
        | LessOrEqual -> 
            let c = RangeConstraint(constraintExpr, System.Double.NegativeInfinity, 0.0)
            solver.Add(c)
        | GreaterOrEqual -> 
            let c = RangeConstraint(constraintExpr, 0.0, System.Double.PositiveInfinity)
            solver.Add(c)


    let private addConstraint (varMap:Map<DecisionName, Variable>) (c:Types.Constraint) (solver:Solver) =
        match c.Expression with
        | Equality (lhs, rhs) -> addEqualityConstraint varMap c.Name lhs rhs solver
        | Inequality (lhs, inequality, rhs) -> addInequalityConstraint varMap c.Name lhs rhs inequality solver


    let private addConstraints (varMap:Map<DecisionName, Variable>) (constraints:List<Types.Constraint>) (solver:Solver) =
        for c in constraints do
            addConstraint varMap c solver |> ignore


    let private buildSolution (decisionMap:Map<DecisionName,Decision>) (varMap:Map<DecisionName, Variable>) (solver:Solver) (objective:Types.Objective) =
        let decisions =
            decisionMap
            |> Map.toSeq
            |> Seq.map (fun (n, d) -> 
                            match Map.tryFind n varMap with 
                            | Some var -> d, var.SolutionValue() 
                            | None -> d, 0.0)
            |> Map.ofSeq

        {
            DecisionResults = decisions
            ObjectiveResult = solver.Objective().BestBound()
        }


    let private writeLPFile (solver:Solver) (filePath:string) =
        let lpFile = solver.ExportModelAsLpFormat(false)
        System.IO.File.WriteAllText(filePath, lpFile)


    let internal solve (solverType:OrToolsSolverType) (settings:SolverSettings) (model:Coffy.Model.Model) =

        let solver = 
            match solverType with
            | CBC -> Solver.CreateSolver("MIP Solver", "CBC_MIXED_INTEGER_PROGRAMMING")
            | GLOP -> Solver.CreateSolver("LP Solver", "GLOP_LINEAR_PROGRAMMING")

        solver.SetTimeLimit(settings.MaxDuration)
        solver.EnableOutput()
    
        let vars = createVariableMap solver model.Decisions
        addConstraints vars model.Constraints solver
        setObjective vars model.Objective solver
    
        // Write LP Formulation to file if requested
        settings.WriteLPFile |> Option.map (writeLPFile solver) |> ignore
    
        let resultStatus = solver.Solve()
    
        match resultStatus with
        | Solver.ResultStatus.OPTIMAL -> 
            buildSolution model.Decisions vars solver model.Objective
            |> SolveResult.Optimal
        | _ ->
            "Unable to find optimal solution"
            |> SolveResult.Suboptimal


[<RequireQualifiedAccess>]
module Solver =

    let solve (settings:SolverSettings) (model:Coffy.Model.Model) =

        match settings.SolverType with
        | CBC -> ORTools.solve ORTools.OrToolsSolverType.CBC settings model
        | GLOP -> ORTools.solve ORTools.OrToolsSolverType.GLOP settings model
        | Cplex128 -> Optano.solve Optano.OptanoSolverType.Cplex128 settings model
        | Gurobi900 -> Optano.solve Optano.OptanoSolverType.Gurobi900 settings model

    

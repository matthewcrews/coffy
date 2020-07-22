namespace Coffy.Types


type DecisionType =
    | Boolean
    | Integer of LowerBound:int * UpperBound:int

type DecisionName = DecisionName of string

type Decision = {
    Name : DecisionName
    Type : DecisionType
}
with

    static member (*) (decision:Decision, i:int) =
        LinearExpression.OfDecision decision * i

    static member (*) (i:int, decision:Decision) =
        LinearExpression.OfDecision decision * i

    static member (+) (decision:Decision, i:int) =
        LinearExpression.OfDecision decision + i

    static member (+) (i:int, decision:Decision) =
        LinearExpression.OfDecision decision + i

    static member (+) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision + rhsDecision

    static member (-) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision + (-1 * rhsDecision)

    static member (-) (decision:Decision, i:int) =
        LinearExpression.OfDecision decision - i

    static member (-) (i:int, decision:Decision) =
        LinearExpression.OfDecision decision - i

    static member (<==) (decision:Decision, i:int) =
        LinearExpression.OfDecision decision <== i

    static member (<==) (i:int, decision:Decision) =
        LinearExpression.OfInt i <== decision

    static member (<==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision <== rhsDecision

    static member (==) (decision:Decision, i:int) =
        LinearExpression.OfDecision decision == i

    static member (==) (i:int, decision:Decision) =
        LinearExpression.OfInt i  == decision

    static member (==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision == rhsDecision

    static member (>==) (decision:Decision, i:int) =
        LinearExpression.OfDecision decision >== i

    static member (>==) (i:int, decision:Decision) =
        LinearExpression.OfInt i >== decision

    static member (>==) (decision:Decision, rhsDecision:Decision) =
        LinearExpression.OfDecision decision >== rhsDecision


and LinearExpression (names:Set<DecisionName>, coefficients : Map<DecisionName, int>, decisions : Map<DecisionName, Decision>, offset:int) =
    member this.Names = names
    member this.Coefficients = coefficients
    member this.Decisions = decisions
    member this.Offset = offset

    static member private Equivalent (lExpr:LinearExpression) (rExpr:LinearExpression) =
        let isEqualOffset = (lExpr.Offset = rExpr.Offset)
        let leftOnlyNames = lExpr.Names - rExpr.Names
        let rightOnlyNames = rExpr.Names - lExpr.Names
        let overlapNames = Set.intersect lExpr.Names rExpr.Names

        let leftOnlyNamesAreZero = 
            leftOnlyNames
            |> Set.forall (fun n -> lExpr.Coefficients.[n] = 0)

        let rightOnlyNamesAreZero =
            rightOnlyNames
            |> Set.forall (fun n -> rExpr.Coefficients.[n] = 0)

        let overlapNamesMatch =
            overlapNames
            |> Set.forall (fun n -> lExpr.Coefficients.[n] = rExpr.Coefficients.[n])

        isEqualOffset && leftOnlyNamesAreZero && rightOnlyNamesAreZero && overlapNamesMatch

    override this.GetHashCode () =
        hash this

    override this.Equals(obj) =
        match obj with
        | :? LinearExpression as expr -> LinearExpression.Equivalent this expr
        | _ -> false

    static member OfInt (i:int) =
        LinearExpression (Set.empty, Map.empty, Map.empty, i)


    static member OfDecision (d:Decision) =
        let names = Set.ofList [d.Name]
        let coefs = Map.ofList [d.Name, 1]
        let decs = Map.ofList [d.Name, d]
        LinearExpression (names, coefs, decs, 0)

    static member GetDecisions (expr:LinearExpression) =
        expr.Decisions
        |> Map.toList
        |> List.map snd
        |> Set.ofList

    static member Zero =
        LinearExpression (Set.empty, Map.empty, Map.empty, 0)

    static member private Merge (l:LinearExpression, r:LinearExpression) =
        // Assume the Left LinearExpression is larget than the right
        let nameOverlap = Set.intersect l.Names r.Names
        
        for n in nameOverlap do
            if l.Decisions.[n].Type <> r.Decisions.[n].Type then
                let (DecisionName name) = n
                invalidArg name "Cannot have mismatched DecisionTypes for same DecisionName"

        let newNames = l.Names + r.Names

        let newDecs = (l.Decisions, (r.Names - l.Names)) ||> Set.fold (fun m k -> Map.add k r.Decisions.[k] m)

        let newCoefs =
            (l.Coefficients, nameOverlap)
            ||> Set.fold (fun m k -> Map.add k (l.Coefficients.[k] + r.Coefficients.[k]) m)
            |> fun updatedCoefs -> Set.fold (fun m n -> Map.add n r.Coefficients.[n] m) updatedCoefs (r.Names - l.Names)

        LinearExpression (newNames, newCoefs, newDecs, l.Offset + r.Offset)

    static member (+) (l:LinearExpression, r:LinearExpression) =
        let lSize = Set.count l.Names
        let rSize = Set.count r.Names

        if lSize > rSize then
            LinearExpression.Merge (l, r)
        else
            LinearExpression.Merge (r, l)

    static member (+) (expr:LinearExpression, i:int) =
        expr + (LinearExpression.OfInt i)

    static member (+) (i:int, expr:LinearExpression) =
        (LinearExpression.OfInt i) + expr

    static member (+) (expr:LinearExpression, decision:Decision) =
        expr + (LinearExpression.OfDecision decision)
    
    static member (+) (decision:Decision, expr:LinearExpression) =
        LinearExpression.OfDecision decision + expr

    static member (*) (expr:LinearExpression, i:int) =
        let newCoefs = Map.map (fun k v -> v * i) expr.Coefficients
        LinearExpression (expr.Names, newCoefs, expr.Decisions, expr.Offset * i)

    static member (*) (i:int, expr:LinearExpression) =
        expr * i

    static member (-) (expr:LinearExpression, i:int) =
        expr + (-1 * i)

    static member (-) (i:int, expr:LinearExpression) =
        i + (-1 * expr)

    static member (-) (expr:LinearExpression, d:Decision) =
        expr + (-1 * d)

    static member (-) (d:Decision, expr:LinearExpression) =
        d + (-1 * expr)

    static member (-) (lExpr:LinearExpression, rExpr:LinearExpression) =
        lExpr + (-1 * rExpr)

    static member (<==) (lhs:LinearExpression, rhs:int) =
        Inequality (lhs, LessOrEqual, LinearExpression.OfInt rhs)

    static member (<==) (lhs:int, rhs:LinearExpression) =
        Inequality (LinearExpression.OfInt lhs, LessOrEqual, rhs)

    static member (<==) (lhs:LinearExpression, rhs:Decision) =
        Inequality (lhs, LessOrEqual, LinearExpression.OfDecision rhs)

    static member (<==) (decision:Decision, expr:LinearExpression) =
        LinearExpression.OfDecision decision <== expr

    static member (<==) (lhs:LinearExpression, rhs:LinearExpression) =
        Inequality (lhs, LessOrEqual, rhs)

    static member (==) (lhs:LinearExpression, rhs:int) =
        Equality (lhs, LinearExpression.OfInt rhs)

    static member (==) (lhs:int, rhs:LinearExpression) =
        Equality (LinearExpression.OfInt lhs, rhs)

    static member (==) (lhs:LinearExpression, rhs:Decision) =
        Equality (lhs, LinearExpression.OfDecision rhs)

    static member (==) (decision:Decision, expr:LinearExpression) =
        LinearExpression.OfDecision decision == expr

    static member (==) (lhs:LinearExpression, rhs:LinearExpression) =
        Equality (lhs, rhs)

    static member (>==) (lhs:LinearExpression, rhs:int) =
        Inequality (lhs, GreaterOrEqual, LinearExpression.OfInt rhs)

    static member (>==) (lhs:int, rhs:LinearExpression) =
        Inequality (LinearExpression.OfInt lhs, GreaterOrEqual, rhs)

    static member (>==) (lhs:LinearExpression, rhs:Decision) =
        Inequality (lhs, GreaterOrEqual, LinearExpression.OfDecision rhs)

    static member (>==) (decision:Decision, expr:LinearExpression) =
        LinearExpression.OfDecision decision >== expr

    static member (>==) (lhs:LinearExpression, rhs:LinearExpression) =
        Inequality (lhs, GreaterOrEqual, rhs)


and Inequality =
    | LessOrEqual
    | GreaterOrEqual


and [<NoComparison>]
ConstraintExpression = 
    | Inequality of LHS:LinearExpression * Inequality * RHS:LinearExpression
    | Equality of LHS:LinearExpression * RHS:LinearExpression

type ConstraintName = ConstraintName of string

type [<NoComparison>]
Constraint = {
    Name : ConstraintName
    Expression : ConstraintExpression
}

type ObjectiveSense =
    | Minimize
    | Maximize

type ObjectiveName = ObjectiveName of string

type [<NoComparison>]
Objective = {
    Name : ObjectiveName
    Sense : ObjectiveSense
    Expression : LinearExpression
}

type Solution = {
    DecisionResults : Map<Decision,float>
    ObjectiveResult : float
}


type SolverSettings = {
    MaxDuration : int64
    WriteLPFile : Option<string>
}

type SolveResult =
    | Optimal of Solution
    | Suboptimal of string
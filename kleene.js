
/*
A mathematical proof-of-concept of Kleene's Theorem in formal language theory.
All conceptual credit is due to David Mix Barrington's "A Mathematical Foundation
for Computer Science".
*/

const LAMBDA = '';

class RegularExpression {
    /**
     * Creates an instance of RegularExpression emulating the associated regular
     * expression in formal language theory.
     *
     * @constructor
     * @param {string} type A string representing the type of the regular
     *   expression: "empty", "char", "union", "cat", or "star"
     * @param {RegularExpression} [first=null] The regular expression's 1st operand
     * @param {RegularExpression} [second=null] The regular expression's 2nd operand
    */
    constructor(type, first = null, second = null) {
        this.type = type;
        this.first = first;
        this.second = second;
        this.cachedDFA = null;
    }
    /**
     * For a string s, creates an instance of RegularExpression emulating the
     * regular expression denoted by s in formal language theory. Regular
     * expressions are defined by the following rules:
     * 1. The string "0" is a regular expression denoting the empty language.
     * 2. Any character a that is NOT "\", "(", ")", "0", "+", "*", or "λ" is a
     *    regular expression denoting the language {a}. The strings "\\", "\(",
     *    "\)", "\0", "\+", "\*", and "\λ" are the regular expressions denoting
     *    the languages {"\"}, {"("}, {")"}, {"0"}, {"+"}, {"*"}, and {"λ"}
     *    respectively.
     * 3. If "S" and "T" are regular expressions denoting languages L(S) and L(T),
     *    then "S+T" is a regular expression denoting the union of L(S) and L(T).
     *    Note that this sense of "+" in formal language theory is different from
     *    the usual sense indicating one or more occurences of the preceding
     *    regular expression.
     * 4. If "S" and "T" are regular expressions denoting languages L(S) and L(T),
     *    then "ST" is a regular expression denoting the concatenation product of
     *    L(S) and L(T). The concatenation product of languages A and B is the
     *    language of all strings uv such that u is in A and v is in B.
     * 5. If "S" is the regular expression denoting the language L(S), "S*" is a
     *    regular expression denoting the Kleene star of L(S). The Kleene star A*
     *    of a language A is defined as follows:
     *    1. LAMBDA is in A*.
     *    2. If u is in A* and v is in A, then the concatenation uv is in A*.
     *    3. The only strings in A* are those given by the above rules.
     * 6. The only regular expressions are those defined by the above rules.
     *
     * @param {string} s The string denoting the regular expression
     * @return {RegularExpression} The regular expression denoted by s
     * @throws {SyntaxError} s must have proper escaping and syntax
    */
    static fromString(s) {
        if (s === LAMBDA) {
            // "0*" is the correct regular expression for LAMBDA but we treat
            // LAMBDA and the unescaped unicode character '\u03BB' as synonyms
            return RegularExpression.fromString('0*');
        }
        // Lexer
        const tokens = [];
        let length = s.length;
        for (let i = 0; i < length; i++) {
            switch (s[i]) {
                case '0':
                    tokens.push(new RegularExpression('empty'));
                    break;
                case '(':
                    tokens.push(new RegularExpression('('));
                    break;
                case ')':
                    tokens.push(new RegularExpression(')'));
                    break;
                case '+':
                    tokens.push(new RegularExpression('union'));
                    break;
                case '*':
                    tokens.push(new RegularExpression('star'));
                    break;
                case '\u03BB':
                    tokens.push(new RegularExpression('lambda'));
                    break;
                case '\\':
                    i++;
                    if (i === length) {
                        throw new SyntaxError('Unexpected end of string');
                    }
                    /* FALLTHROUGH */
                default:
                    tokens.push(new RegularExpression('char', s[i]));
            }
        }
        // Parser
        // Based on https://crockford.com/javascript/tdop/tdop.html
        let token;
        let tokenIndex = 0;
        length = tokens.length;

        function typeToBP(type) {
            switch (type) {
                case 'end': return 0;
                case ')': return 0;
                case 'union': return 10;
                case '(': return 20;
                case 'lambda': return 20;
                case 'empty': return 20;
                case 'char': return 20;
                case 'cat': return 20;
                case 'star': return 30;
                default:
                    throw new SyntaxError('Unexpected ' + type);
            }
        }

        function advance() {
            if (tokenIndex >= length) {
                token = new RegularExpression('end');
            } else {
                token = tokens[tokenIndex];
                tokenIndex++;
            }
        }

        function buildRegExp(rightBindingPower) {
            let left, t = token;
            advance();
            // Consider t as a 0, char, or expression in parenthesis
            switch (t.type) {
                case 'lambda':
                    t = RegularExpression.fromString(LAMBDA);
                    /* FALLTHROUGH */
                case '(':
                    // Only do this when it's not fallthrough
                    if (t.type === '(') {
                        t = buildRegExp(0);
                        if (token.type !== ')') {
                            throw new SyntaxError('Unexpected ' + token.type);
                        }
                        advance();
                    }
                    /* FALLTHROUGH */
                case 'empty':
                case 'char':
                    // Handle concatenation
                    switch (token.type) {
                        case 'empty':
                        case 'char':
                        case '(':
                            left = new RegularExpression(
                                'cat', t, buildRegExp(20)
                            );
                            break;
                        default:
                            left = t;
                    }
                    break;
                default:
                    throw new SyntaxError('Unexpected ' + t.type);
            }
            while (rightBindingPower < typeToBP(token.type)) {
                t = token;
                advance();
                // Consider t as a + or *
                switch (t.type) {
                    case 'union':
                        t.first = left;
                        t.second = buildRegExp(typeToBP('union') - 1);
                        left = t;
                        break;
                    case 'star':
                        t.first = left;
                        left = t;
                        if (token.type === '(' || token.type === 'empty'
                            || token.type === 'char'
                            || token.type === 'lambda') {
                            left = new RegularExpression(
                                'cat', left, buildRegExp(20)
                            );
                        }
                        break;
                    default:
                        throw new SyntaxError('Unexpected ' + t.type);
                }
            }
            return left;
        }

        advance();

        return buildRegExp(0);
    }
    /**
     * Returns a string representation s of the regular expression such that
     * RegularExpression.fromString(s) produces an equivalent regular
     * expression.
     *
     * @return {string} The regular expression's string representation
    */
    toString() {
        if (this.isLambda()) {
            return '\u03BB';
        }
        let first, second, firstString, secondString;
        switch (this.type) {
            case 'empty': return '0';
            case 'char':
                first = this.first;
                return first === '\\' || first === '(' || first === ')'
                    || first === '0' || first === '+' || first === '*'
                    ? '\\' + first : first;
            case 'union': return this.first.toString() + '+' + this.second.toString();
            case 'cat':
                first = this.first;
                firstString = first.toString();
                if (first.type === 'union') {
                    firstString = '(' + firstString + ')'; // Preserve precedence
                }
                second = this.second;
                secondString = second.toString();
                if (second.type === 'union') {
                    secondString = '(' + secondString + ')'; // Preserve precedence
                }
                return firstString + secondString;
            case 'star':
                first = this.first;
                firstString = first.toString();
                if (first.type === 'union' || first.type === 'cat') {
                    firstString = '(' + firstString + ')'; // Preserve precedence
                }
                return firstString + '*';
        }
    }
    /**
     * Recursively builds a LAMBDA-NFA that decides the same language that is decided
     * by the regular expression. Keeps LAMBDA-moves to a minimum by only building
     * LAMBDA-NFAs with the following properties:
     * 1. The LAMBDA-NFA has exactly one final state (which is not the initial state).
     * 2. There are no transitions into the initial state.
     * 3. There are no transitions out of the final state.
     * 4. The LAMBDA-NFA's alphabet will include every base case char present in the
     *    the regular expression. If the optional parameter sigma is given, every
     *    character a in sigma will also be included in the LAMBDA-NFA's alphabet,
     *    even if a is not necessarily present as a base case char in the
     *    regular expression.
     * This method implements the regular expression to LAMBDA-NFA algorithm in David
     * Mix Barrington's "A Mathematical Foundation for Computer Science" (14.8
     * "Constructing LAMBDA-NFA's from Regular Expressions").
     *
     * @param {Set<char>} [sigma={}] Chars to be added to LAMBDA-NFA's alphabet
     * @return {LambdaNFA} A LAMBDA-NFA satisfying the above properties
    */
    toLambdaNFA(sigma = new Set()) {
        let states, initialState, transitions, finalState, m1, m2, m2init,
            m2transitions, m2transLength, m2transition;

        function pushNoDuplicates(transitions, newTransition) {
            const transLength = transitions.length;
            for (let i = 0; i < transLength; i++) {
                const transition = transitions[i];
                if (transition.fromState === newTransition.fromState
                    && transition.str === newTransition.str
                    && transition.toState === newTransition.toState) {
                    return;
                }
            }
            transitions.push(newTransition);
        }

        switch (this.type) {
            case 'empty':
                initialState = new State();
                finalState = new State();
                states = [initialState, finalState];
                transitions = [];
                break;
            case 'char':
                const char = this.first;
                sigma.add(char);
                initialState = new State();
                finalState = new State();
                states = [initialState, finalState];
                transitions = [new Transition(initialState, char, finalState)];
                break;
            case 'union':
                // m1 and m2 are the LAMBDA-NFAs for the union's operands
                m1 = this.first.toLambdaNFA(sigma);
                m2 = this.second.toLambdaNFA(sigma);
                // Merge m1 and m2's initial and final states and add transitions
                initialState = m1.initialState;
                finalState = m1.finalStates[0];
                states = m1.states.concat(m2.states);
                transitions = m1.transitions;
                m2init = m2.initialState;
                const m2final = m2.finalStates[0];
                states.splice(states.indexOf(m2init), 1);
                states.splice(states.indexOf(m2final), 1);
                m2transitions = m2.transitions;
                m2transLength = m2transitions.length;
                for (let i = 0; i < m2transLength; i++) {
                    m2transition = m2transitions[i];
                    if (m2transition.fromState === m2init) {
                        m2transition.fromState = initialState;
                    }
                    if (m2transition.toState === m2final) {
                        m2transition.toState = finalState;
                    }
                    // The 2nd property means there's no need to worry about
                    // transitions into m2's initial state and out of m2's
                    // final state
                    pushNoDuplicates(transitions, m2transition);
                }
                break;
            case 'cat':
                // m1 and m2 are the LAMBDA-NFAs for the union's operands
                m1 = this.first.toLambdaNFA(sigma);
                m2 = this.second.toLambdaNFA(sigma);
                // Merge m1's final and m2's initial states and add transitions
                initialState = m1.initialState;
                finalState = m2.finalStates[0];
                states = m1.states.concat(m2.states);
                transitions = m1.transitions;
                m2init = m2.initialState;
                const m1final = m1.finalStates[0];
                states.splice(states.indexOf(m2init), 1);
                m2transitions = m2.transitions;
                m2transLength = m2transitions.length;
                for (let i = 0; i < m2transLength; i++) {
                    const m2transition = m2transitions[i];
                    if (m2transition.fromState === m2init) {
                        m2transition.fromState = m1final;
                    }
                    // The 2nd property means there's no need to worry about
                    // transitions into m2's initial state
                    pushNoDuplicates(transitions, m2transition);
                }
                break;
            case 'star':
                // The only operation which requires LAMBDA-moves
                initialState = new State();
                finalState = new State();
                const m = this.first.toLambdaNFA(sigma);
                states = m.states;
                states.push(initialState, finalState);
                transitions = m.transitions;
                const newTransitions = [
                    new Transition(initialState, LAMBDA, m.initialState),
                    new Transition(m.initialState, LAMBDA, m.finalStates[0]),
                    new Transition(m.finalStates[0], LAMBDA, m.initialState),
                    new Transition(m.finalStates[0], LAMBDA, finalState)
                ];
                for (let i = 0; i < 4; i++) {
                    pushNoDuplicates(transitions, newTransitions[i]);
                }
        }
        return new LambdaNFA(sigma, states, initialState, transitions, [finalState]);
    }
    toNFA(sigma = new Set()) {
        return this.toLambdaNFA(sigma).toNFA();
    }
    toDFA(sigma = new Set()) {
        return this.toNFA(sigma).toDFA();
    }
    /**
     * Decides whether or not a given string is in the regular expression's
     * language.
     *
     * @param {string} s The string
     * @return {boolean} Whether or not the string is in the language
    */
    decide(s) {
        let cachedDFA = this.cachedDFA;
        if (cachedDFA === null) { // Don't build DFA more than once
            cachedDFA = this.cachedDFA = this.toDFA();
        }
        return cachedDFA.decide(s);
    }
    isLambda() {
        return this.type === 'star' && this.first.type === 'empty';
    }
}

let stateCounter = 1;

class State {
    constructor() {
        this.id = stateCounter;
        stateCounter++;
    }
    toString() {
        return '' + this.id;
    }
    static resetCounter() {
        stateCounter = 1;
    }
}

class Transition {
    constructor(fromState, str, toState) {
        this.fromState = fromState;
        this.str = str;
        this.toState = toState;
    }
    toString() {
        return '(' + this.fromState.toString() + ', ' + (this.str === LAMBDA
            ? '\u03BB' : this.str) + ', ' + this.toState.toString() + ')';
    }
}

class FSA {
    constructor(sigma, states, initialState, transitions, finalStates) {
        this.sigma = sigma;
        this.states = states;
        this.initialState = initialState;
        this.transitions = transitions;
        this.finalStates = finalStates;
        this.cachedDFA = null;
    }
    /**
     * Builds a regular expression that denotes the same language that is
     * decided by the FSA. This method implements the FSA to regular
     * expression algorithm in David Mix Barrington's "A Mathematical
     * Foundation for Computer Science" (14.10 "State Elimination: NFA's
     * into Regular Expressions").
     *
     * @return {RegularExpression} Denotes the FSA's language.
    */
    toRegularExpression() {
        // Build a RE-NFA from the FSA. A RE-NFA is an FSA which transitions
        // on regular expressions instead of on characters or LAMBDA. This
        // RE-NFA will satisfy the following properties:
        // 1. The RE-NFA has exactly one final state (that is not init state).
        // 2. There are no transitions into the initial state.
        // 3. There are no transitions out of the final state.
        const reStates = this.states.slice(); // Shallow copy
        const reFinalStates = this.finalStates.slice(); // Shallow copy
        let reTransitions = []; // Transitions for RE-NFA
        // Replace parallel edges (p, R_1, q), ..., (p, R_n, q) with a single
        // edge (p, R, q) where R = R_1 + ... + R_n.
        const transitions = this.transitions.slice(); // Shallow copy
        for (let i = 0; i < transitions.length; i++) {
            const t = transitions[i];
            const fromState = t.fromState;
            const toState = t.toState;
            let re = RegularExpression.fromString(t.str);
            for (let j = i + 1; j < transitions.length; j++) {
                const t2 = transitions[j];
                if (t2.fromState === fromState
                    && t2.toState === toState) {
                    const re2 = RegularExpression.fromString(t2.str);
                    if (re.isLambda() && re2.type === 'star') {
                        re = re2; // re2 is a superset of re
                    } else if (re.type !== 'star' || !re2.isLambda()) {
                        re = new RegularExpression('union',
                            re, re2
                        );
                    }
                    transitions.splice(j, 1);
                }
            }
            reTransitions.push(new Transition(
                fromState, re, toState
            ));
        }

        // If the initial state is final or there are any transitions into
        // the initial state, make a new initial state and add a LAMBDA-move
        // from the new initial state to the old initial state.
        let reInitialState = this.initialState;
        if (reFinalStates.indexOf(reInitialState) !== -1) {
            const newInitialState = new State();
            reStates.push(newInitialState);
            reTransitions.push(new Transition(
                newInitialState,
                RegularExpression.fromString(LAMBDA),
                reInitialState
            ));
            reInitialState = newInitialState;
        } else {
            let reTransLength = reTransitions.length;
            for (let i = 0; i < reTransLength; i++) {
                if (reTransitions[i].toState === reInitialState) {
                    const newInitialState = new State();
                    reStates.push(newInitialState);
                    reTransitions.push(new Transition(
                        newInitialState,
                        RegularExpression.fromString(LAMBDA),
                        reInitialState
                    ));
                    reInitialState = newInitialState;
                    break;
                }
            }
        }
        // If there is more than one final state or there are any
        // transitions out of the final state, make a new final state and
        // add a LAMBDA-move from every old final state to the new one.
        let reFinalState;
        const fsl = reFinalStates.length;
        if (fsl > 1) {
            reFinalState = new State();
            reStates.push(reFinalState);
            for (let i = 0; i < fsl; i++) {
                reTransitions.push(new Transition(
                    reFinalStates[i],
                    RegularExpression.fromString(LAMBDA),
                    reFinalState
                ));
            }
        } else if (fsl === 1) {
            const currReFinalState = reFinalStates[0];
            reFinalState = currReFinalState;
            const reTransLength = reTransitions.length;
            for (let i = 0; i < reTransLength; i++) {
                if (reTransitions[i].fromState === currReFinalState) {
                    reFinalState = new State();
                    reStates.push(reFinalState);
                    reTransitions.push(new Transition(
                        currReFinalState,
                        RegularExpression.fromString(LAMBDA),
                        reFinalState
                    ));
                    break;
                }
            }
        } else {
            reFinalState = new State(); // No transitions to final state
            reStates.push(reFinalState);
        }

        let reStatesLength = reStates.length;
        while (reStatesLength > 2) {
            // Find a state which is not the initial or final state
            let q; // State to eliminate
            for (let i = 0; i < 3; i++) {
                const s = reStates[i];
                if (s !== reInitialState && s !== reFinalState) {
                    q = s;
                    break;
                }
            }
            // For any pair of transitions (p, alpha, q), (q, gamma, r), add
            // the transition (p, alpha beta* gamma, r) where (q, beta, q)
            // is the loop at q. If there is no loop, set beta=LAMBDA.
            // Remove q and any transitions involving q from the RE-NFA.
            const transFromQ = [];
            const transToQ = [];
            let betaStar = RegularExpression.fromString(LAMBDA);
            for (let i = 0; i < reTransitions.length; i++) {
                const t = reTransitions[i];
                if (t.fromState === q) {
                    if (t.toState === q) {
                        betaStar = t.str; // Loop at q
                    } else {
                        transFromQ.push(t);
                    }
                    reTransitions.splice(i, 1);
                    i -= 1;
                } else if (t.toState === q) {
                    transToQ.push(t);
                    reTransitions.splice(i, 1);
                    i -= 1;
                }
            }
            if (betaStar.type !== 'star') { // R** = R*
                betaStar = new RegularExpression('star', betaStar);
            }
            const tfqLength = transFromQ.length;
            const ttqLength = transToQ.length;
            for (let i = 0; i < ttqLength; i++) {
                const t1 = transToQ[i];
                const p = t1.fromState;
                const alpha = t1.str;
                for (let j = 0; j < tfqLength; j++) {
                    const t2 = transFromQ[j];
                    const r = t2.toState;
                    const gamma = t2.str;
                    // Simplify regular expression before adding
                    if (alpha.isLambda()) {
                        if (betaStar.isLambda()) {
                            reTransitions.push(new Transition(
                                p, gamma, r)
                            );
                        } else if (gamma.isLambda()) {
                            reTransitions.push(new Transition(
                                p, betaStar, r)
                            );
                        } else {
                            reTransitions.push(new Transition(p,
                                new RegularExpression('cat', betaStar, gamma),
                            r));
                        }
                    } else if (betaStar.isLambda()) {
                        if (gamma.isLambda()) {
                            reTransitions.push(new Transition(
                                p, alpha, r)
                            );
                        } else {
                            reTransitions.push(new Transition(p,
                                new RegularExpression('cat', alpha, gamma),
                            r));
                        }
                    } else if (gamma.isLambda()) {
                        reTransitions.push(new Transition(p,
                            new RegularExpression('cat', alpha, betaStar),
                        r));
                    } else {
                        reTransitions.push(new Transition(p,
                            new RegularExpression('cat', alpha,
                                new RegularExpression('cat',
                                    betaStar, gamma
                                )
                            ), r
                        ));
                    }
                }
            }
            reStates.splice(reStates.indexOf(q), 1);
            reStatesLength--;
            // Replace parallel edges (p, R_1, q), ..., (p, R_n, q) with a single
            // edge (p, R, q) where R = R_1 + ... + R_n.
            const newReTransitions = [];
            for (let i = 0; i < reTransitions.length; i++) {
                const t = reTransitions[i];
                const fromState = t.fromState;
                const toState = t.toState;
                let re = t.str;
                for (let j = i + 1; j < reTransitions.length; j++) {
                    const t2 = reTransitions[j];
                    if (t2.fromState === fromState
                        && t2.toState === toState) {
                        re = new RegularExpression('union',
                            re, t2.str
                        );
                        reTransitions.splice(j, 1);
                    }
                }
                newReTransitions.push(new Transition(
                    fromState, re, toState
                ));
            }
            reTransitions = newReTransitions;
        }
        // The only possible transition is now from the initial state to the
        // final state, so that transition's value is a regular expression
        // that decides the same language that is decided by the FSA
        return reTransitions[0].str;
    }
    toString() {
        const sigma = this.sigma;
        const sigmaArray = [];
        for (const char of sigma) {
            sigmaArray.push(char);
        }

        const stateToNatMap = new Map();
        const transitions = this.transitions;
        const transLength = transitions.length;

        let counter = 1;
        function buildStateToNatMap(state) {
            stateToNatMap.set(state, counter);
            counter += 1;
            for (let i = 0; i < transLength; i++) {
                const transition = transitions[i];
                if (transition.fromState === state
                    && !stateToNatMap.has(transition.toState)) {
                    buildStateToNatMap(transition.toState);
                }
            }
        }

        buildStateToNatMap(this.initialState);

        function statesToNats(states) {
            const length = states.length;
            const nats = Array(length);
            for (let i = 0; i < length; i++) {
                nats[i] = stateToNatMap.get(states[i]);
            }
            nats.sort((a, b) => a - b);
            return nats.join(', ');
        }

        let transNats = [];
        for (let i = 0; i < transLength; i++) {
            const transition = transitions[i];
            transNats.push([
                stateToNatMap.get(transition.fromState),
                transition.str,
                stateToNatMap.get(transition.toState)
            ]);
        }

        transNats.sort(
            ([f1, c1, t1,], [f2, c2, t2]) => {
                let f = f1 - f2;
                if (f !== 0) return f;
                if (c1 < c2) return -1;
                if (c1 > c2) return 1;
                return t1 - t2;
            }
        );

        transNats = transNats.map(([f, c, t]) => `(${f}, ${
            c === LAMBDA ? '\u03BB' : c
        }, ${t})`).join(', ');

        return `Alphabet: {${sigmaArray.join(', ')}}
States: {${statesToNats(this.states)}}
Initial State: 1
Transitions: {${transNats}}
Final States: {${statesToNats(this.finalStates)}}`;
    }
    /**
     * Decides whether or not a given string is in the FSA's language.
     *
     * @param {string} s The string
     * @return {boolean} Whether or not the string is in the language
    */
    decide(s) {
        let cachedDFA = this.cachedDFA;
        if (cachedDFA === null) { // Don't build DFA more than once
            cachedDFA = this.cachedDFA = this.toDFA();
        }
        return cachedDFA.decide(s);
    }
}

class LambdaNFA extends FSA {
    /**
     * Builds an NFA that decides the same language that is decided by the
     * LAMBDA-NFA. This method is implemented far from optimally from the
     * perspective of time complexity, but should run reasonably for small
     * LAMBDA-NFAs. This method implements the LAMBDA-NFA to NFA algorithm
     * in David Mix Barrington's "A Mathematical Foundation for Computer
     * Science" (14.7 "Killing LAMBDA-moves: LAMBDA-NFA's into NFA's").
     *
     * @return {NFA} An NFA that decides the LAMBDA-NFA's language
    */
    toNFA() {
        const charTransitions = [];
        const transitions = this.transitions;
        const length = transitions.length;

        function statesTo(state, visitedStates) {
            visitedStates.add(state);
            for (let i = 0; i < length; i++) {
                const transition = transitions[i];
                if (transition.str === LAMBDA && transition.toState === state) {
                    const fromState = transition.fromState;
                    if (!visitedStates.has(fromState)) {
                        statesTo(fromState, visitedStates);
                    }
                }
            }
            return visitedStates;
        }

        function statesFrom(state, visitedStates) {
            visitedStates.add(state);
            for (let i = 0; i < length; i++) {
                const transition = transitions[i];
                if (transition.str === LAMBDA && transition.fromState === state) {
                    const toState = transition.toState;
                    if (!visitedStates.has(toState)) {
                        statesFrom(toState, visitedStates);
                    }
                }
            }
            return visitedStates;
        }

        for (let i = 0; i < length; i++) {
            const transition = transitions[i];
            const char = transition.str;
            if (char !== LAMBDA) {
                // The set of states that can reach the transition's from state
                // by taking LAMBDA-moves
                const statesToFrom = statesTo(transition.fromState, new Set());
                // The set of states that can be reached by the transition's to
                // state by taking LAMBDA-moves
                const statesFromTo = statesFrom(transition.toState, new Set());
                for (const fromState of statesToFrom) {
                    for (const toState of statesFromTo) {
                        charTransitions.push(new Transition(
                            fromState, char, toState
                        ));
                    }
                }
            }
        }

        // Every state with LAMBDA-moves to a state that is final in the
        // LAMBDA-NFA is final in the NFA
        const finalStateSet = new Set();
        const lambdaNFAFinalStates = this.finalStates;
        const lambdaNFAFinalStatesLength = lambdaNFAFinalStates.length;
        for (let i = 0; i < lambdaNFAFinalStatesLength; i++) {
            statesTo(lambdaNFAFinalStates[i], finalStateSet);
        }
        const finalStates = [];
        for (const finalState of finalStateSet) {
            finalStates.push(finalState);
        }

        return new NFA(this.sigma, this.states,
            this.initialState, charTransitions, finalStates);
    }
    toDFA() {
        return this.toNFA().toDFA();
    }
}

class NFA extends LambdaNFA {
    /**
     * Builds a DFA that decides the same language that is decided by the
     * NFA. This method is implemented far from optimally from the
     * perspective of time complexity, but should run reasonably for small
     * NFAs. This method implements the NFA-to-DFA algorithm in David Mix
     * Barrington's "A Mathematical Foundation for Computer Science" (14.6
     * "The Subset Construction: NFA's into DFA's").
     *
     * @return {DFA} A DFA that decides the NFA's language
    */
    toDFA() {
        const sigma = this.sigma;
        const nfaStateSets = [];
        const nfaStateSetTransitions = [];
        const transitions = this.transitions;
        const transLength = transitions.length;

        function stateSetsFrom(fromSet) {
            nfaStateSets.push(fromSet);
            for (const char of sigma) {
                const toSet = new Set();
                for (let i = 0; i < transLength; i++) {
                    const trans = transitions[i];
                    if (trans.str === char && fromSet.has(trans.fromState)) {
                        toSet.add(trans.toState);
                    }
                }
                // Check whether or not toSet has been observed
                const toSetSize = toSet.size;
                const nfaStateSetsLength = nfaStateSets.length;
                let toSetNotYetObserved = true;
                stateSetLoop: for (let i = 0; i < nfaStateSetsLength; i++) {
                    const nfaStateSet = nfaStateSets[i];
                    if (nfaStateSet.size !== toSetSize) continue stateSetLoop;
                    for (const state of nfaStateSet) {
                        if (!toSet.has(state)) {
                            continue stateSetLoop;
                        }
                    }
                    toSetNotYetObserved = false;
                    break;
                }
                if (toSetNotYetObserved) {
                    stateSetsFrom(toSet); // Recursively (and lazily) build DFA
                }
                nfaStateSetTransitions.push(new Transition(
                    fromSet, char, toSet
                ));
            }
        }

        let initialStateSet = new Set([this.initialState]);

        stateSetsFrom(initialStateSet);

        let initialState = null;
        const finalStates = [];

        const nfaFinalStates = this.finalStates;
        const nfaFinalStatesLength = nfaFinalStates.length;
        const nfaStateSetsLength = nfaStateSets.length;
        const nfaStateSetTransLength = nfaStateSetTransitions.length;
        for (let i = 0; i < nfaStateSetsLength; i++) {
            const stateSet = nfaStateSets[i];
            const state = new State();
            if (stateSet === initialStateSet) {
                initialState = state;
            }
            for (let j = 0; j < nfaFinalStatesLength; j++) {
                if (stateSet.has(nfaFinalStates[j])) {
                    finalStates.push(state);
                    break;
                }
            }
            nfaStateSets[i] = state;
            const stateSetSize = stateSet.size;
            for (let j = 0; j < nfaStateSetTransLength; j++) {
                const transition = nfaStateSetTransitions[j];
                const fromState = transition.fromState;
                if (fromState.size === stateSetSize) {
                    let sameStateSet = true;
                    for (const s of fromState) {
                        if (!stateSet.has(s)) {
                            sameStateSet = false;
                            break;
                        }
                    }
                    if (sameStateSet) {
                        transition.fromState = state;
                    }
                }
                const toState = transition.toState;
                if (toState.size === stateSetSize) {
                    let sameStateSet = true;
                    for (const s of toState) {
                        if (!stateSet.has(s)) {
                            sameStateSet = false;
                            break;
                        }
                    }
                    if (sameStateSet) {
                        transition.toState = state;
                    }
                }
            }
        }

        return new DFA(
            sigma, nfaStateSets, initialState,
            nfaStateSetTransitions, finalStates
        );
    }
}

class DFA extends NFA {
    /**
     * Builds a minimal DFA that decides the same language that is decided
     * by the DFA. The minimal DFA for a language is the DFA that decides
     * it with as few states as possible. This method implements the
     * minimization algorithm in David Mix Barrington's "A Mathematical
     * Foundation for Computer Science" (14.3.3 "Minimizing DFA's").
     *
     * @return {DFA} The minimal DFA
    */
    minimize() {
        const sigma = this.sigma;
        const transitions = this.transitions;
        const transLength = transitions.length;
        const states = this.states;
        const statesLength = states.length;

        const acceptingStates = new Set(this.finalStates);
        const rejectingStates = new Set();
        for (let i = 0; i < statesLength; i++) {
            const state = states[i];
            if (!acceptingStates.has(state)) {
                rejectingStates.add(state);
            }
        }

        // Accepting and rejecting states must be L-inequivalent
        let partition = [acceptingStates, rejectingStates];

        // Revise partition until partition 
        let partitionGivesEquivalenceClasses;
        do {
            partitionGivesEquivalenceClasses = true;

            // Build a new partition from the current partition, separating
            // state subsets found to be L-inequivalent. If no new separation
            // occurs, then the current partition gives the L-equivalence
            // classes.
            const newPartition = [];
            const partitionLength = partition.length;
            // Loop through every state set in the current partition
            outer: for (let i = 0; i < partitionLength; i++) {
                const stateSet = partition[i];
                for (let c of sigma) {
                    for (let j = 0; j < partitionLength; j++) {
                        const toStateSet = partition[j];
                        // Subset of states in the state set which have a
                        // c-move to a state in the to-state set
                        const goToToStateSet = new Set();
                        // Populate the go-to-to-state set
                        for (let k = 0; k < transLength; k++) {
                            const t = transitions[k];
                            if (t.str !== c || !toStateSet.has(t.toState)) {
                                continue;
                            }
                            if (stateSet.has(t.fromState)) {
                                goToToStateSet.add(t.fromState);
                            }
                        }
                        // For every state in the state set to be L-
                        // equivalent, every one of them must go to the
                        // same state set, so they must all go to the to-
                        // state-set, or none of them can go.
                        if (goToToStateSet.size > 0
                            && goToToStateSet.size < stateSet.size) {
                            partitionGivesEquivalenceClasses = false;
                            // Subset of states in the state set which do not
                            // have a c-move to a state in the to-state set
                            const dontGoToToStateSet = new Set();
                            for (const s of stateSet) {
                                if (!goToToStateSet.has(s)) {
                                    dontGoToToStateSet.add(s);
                                }
                            }
                            newPartition.push(
                                goToToStateSet,
                                dontGoToToStateSet
                            );
                            continue outer;
                        }
                    }
                } // Every state in the state set is L-equivalent
                newPartition.push(stateSet);
            }
            partition = newPartition;
        } while (!partitionGivesEquivalenceClasses);

        // Find final state sets and transitions between sets
        const finalSets = [];
        const setTransitions = [];
        const partitionLength = partition.length;
        for (let i = 0; i < partitionLength; i++) {
            const stateSet = partition[i];
            for (const s of acceptingStates) {
                if (stateSet.has(s)) {
                    finalSets.push(stateSet);
                    break; // If one is final, all must be final
                }
            }
            for (const c of sigma) {
                const toSet = new Set();
                for (let j = 0; j < transLength; j++) {
                    const t = transitions[j];
                    if (t.str === c
                        && stateSet.has(t.fromState)) {
                        toSet.add(t.toState);
                    }
                }
                toLoop: for (let j = 0; j < partitionLength; j++) {
                    const maybeToSet = partition[j];
                    for (const s of toSet) {
                        if (!maybeToSet.has(s)) {
                            continue toLoop;
                        }
                    }
                    // maybeToSet must be the set the
                    // state set goes to
                    setTransitions.push(new Transition(
                        stateSet, c, maybeToSet
                    ));
                }
            }
        }
        let initState;
        // Replace state sets with regular states
        for (let i = 0; i < partitionLength; i++) {
            const stateSet = partition[i];
            const stateSetSize = stateSet.size;
            const state = new State();
            partition[i] = state;
            if (stateSet.has(this.initialState)) {
                initState = state;
            }
            for (let j = 0; j < setTransitions.length; j++) {
                const t = setTransitions[j];
                const fromState = t.fromState;
                if (fromState.size === stateSetSize) {
                    let sameStateSet = true;
                    for (const s of fromState) {
                        if (!stateSet.has(s)) {
                            sameStateSet = false;
                            break;
                        }
                    }
                    if (sameStateSet) {
                        t.fromState = state;
                    }
                }
                const toState = t.toState;
                if (toState.size === stateSetSize) {
                    let sameStateSet = true;
                    for (const s of toState) {
                        if (!stateSet.has(s)) {
                            sameStateSet = false;
                            break;
                        }
                    }
                    if (sameStateSet) {
                        t.toState = state;
                    }
                }
            }
            finalLoop: for (let j = 0; j < finalSets.length; j++) {
                if (finalSets[j].size === stateSetSize) {
                    for (const s of finalSets[j]) {
                        if (!stateSet.has(s)) {
                            sameStateSet = false;
                            continue finalLoop;
                        }
                    }
                    finalSets[j] = state;
                }
            }
        }

        return new DFA(sigma, partition,
            initState, setTransitions, finalSets);
    }
    decide(s) {
        let currentState = this.initialState;
        const length = s.length;
        const transitions = this.transitions;
        const transLength = transitions.length;
        for (let i = 0; i < length; i++) {
            const char = s[i];
            if (!this.sigma.has(char)) {
                throw new Error(char + ' not in DFA alphabet');
            }
            for (let j = 0; j < transLength; j++) {
                const transition = transitions[j];
                if (transition.fromState === currentState && transition.str == char) {
                    currentState = transition.toState;
                    break;
                }
            }
        }
        return this.finalStates.indexOf(currentState) !== -1;
    }
}

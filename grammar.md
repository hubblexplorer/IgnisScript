$$
\begin{align}
[\text{Prog}] &\to [\text{Stmt}]^* \\\\
[\text{Stmt}] &\to  
\begin{cases}
    \text{exit}([\text{Expr}]) \text{;} \\
    \text{print}([\text{Expr}]) \text{;} \\ 
    \text{println}([\text{Expr}]) \text{;} \\
    \text{let}\space\text{ident} : [\text{Type}] = [\text{Expr}] \text{;} \\
    \text{if} ([\text{Expr}].\text{bool})\space[\text{Scope}] \\
    \text{else if} ([\text{Expr}].\text{bool})\space[\text{Scope}] \\
    \text{else}\space [\text{Scope}] \\
    [\text{Scope}]\\
    \text{ident} = [\text{Expr}] \text{;}\space \in \space \text{ident}.\text{Type} = [\text{Expr}]  \\
    \text{while} ([\text{Expr}].\text{bool}) [\text{Scope}] \\

\end{cases}\\ \\

\text{[Scope]} &\to \{[\text{Stmt}]^*\}\\\\

[\text{Expr}] &\to 
\begin{cases}
    [\text{Term}] \\
    [\text{BinExpr}] \\
    [\text{UnExpr}] \\
\end{cases} \\
\\
[\text{BinExpr}] &\to 
\begin{cases}
    [\text{Expr}] * [\text{Expr}] & \text{prec} = 1 & [\text{Type}] = int \\
    [\text{Expr}]\space / \space [\text{Expr}] & \text{prec} = 1 & [\text{Type}] = int\\
    [\text{Expr}] + [\text{Expr}] & \text{prec} = 0 & [\text{Type}] = int\\
    [\text{Expr}] - [\text{Expr}] & \text{prec} = 0 & [\text{Type}] = int\\
    [\text{Expr}]\space \&\& \space[\text{Expr}] & \text{prec} = 0 & [\text{Type}] = bool \\
    [\text{Expr}]\space || \space [\text{Expr}] & \text{prec} = 0 & [\text{Type}] = bool \\
    [\text{Expr}] < [\text{Expr}] & \text{prec} = 0 & [\text{Type}] = bool \\
    [\text{Expr}] > [\text{Expr}] & \text{prec} = 0 & [\text{Type}] = bool \\
    [\text{Expr}] <= [\text{Expr}] & \text{prec} = 0 & [\text{Type}] = bool \\
    [\text{Expr}] >= [\text{Expr}] & \text{prec} = 0 & [\text{Type}] = bool \\
    [\text{Expr}] == [\text{Expr}] & \text{prec} = 0 & [\text{Type}] = bool \\
\end{cases} \\
\\
[\text{UnExpr}] &\to 
\begin{cases}
    -[\text{Expr}].\text{int}\\
    ![\text{Expr}].\text{bool}\\
\end{cases} \\
\\
[\text{Term}] &\to 
\begin{cases}
    \text{int\_lit}.\text{int} \\
    \text{bool\_lit}.\text{bool} \\
    \text{char\_lit}.\text{char} \\
    \text{ident}.[\text{Type}] \\
    [\text{Expr}].[\text{Type}] 
\end{cases} \\
\\
[\text{Type}] &\to 
\begin{cases}
    \text{int} \\
    \text{bool} \\
    \text{char}
\end{cases} \\



\end{align}

$$
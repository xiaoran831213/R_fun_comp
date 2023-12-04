#' Function Composition
#'
#' Given calls `f(.,  a=A)` and `g(., b=B)`, compose  `function(.)  g(f(., a=A),
#' b=B)`.
#'
#' By default,  `fcp` is asigned  to operator `%.%` so  a final function  can be
#' constructed  from a  chain of  more  than two  calls,  just like  a chain  of
#' [|>][base::pipeOp] without input or a  chain of [%>%][magrittr::%>%] with `.`
#' as the special input.
#'
#' `fcp` is best  used to quickly create anonymous functions  from existing ones
#' as input of functional  programming tools like [base::apply], [base::sapply],
#' and [base::lapply], etc. Should the composed function be saved for reuse, its
#' body shall retain the original syntax  of component function calls as closely
#' as possible.
#'
#' Like [%>%][magrittr::%>%], `fcp` allows a downstream call to use `.` to refer
#' to input from its  immediate upstream call, and pipe the  input into any slot
#' in the downstream call's list of  arguments while transforming the same input
#' through expressions  of `.`; when  no transformation is needed,  `fcp` allows
#' unchanged input  be piped into  all empty slots  in the argument  list (e.g.,
#' fun(x=1, y=, z=2) for named slots, or fun(1, , z=2) for positinal slots).
#'
#' Like [%>%][magrittr::%>%],  when no explicit  reference to the output  of the
#' uptream call  is used,  the results  is then piped  into the  first available
#' positional argument of the downstream call, and  a call with a lone input can
#' be shorterned to function names without parentheses.
#' 
#' Later calls in a `fcp` chain can refer to earlier calls by numerical symbols,
#' so `n` refers to the result of nth call in the chain and`0` refers to initial
#' input, allowing more flexablity than a strice "upstream to downstream" pipe.
#'
#' @usage
#' fcp(f, g) # or f %.% g
#'
#' @param f left--operand: inner call to giver a result first.
#' @param g right-operand: outer call to receirve that result.
#' @return composite function g(f(x, ...), ...)
#' @examples
#' ## ex1: match and extract date (pipe the initial input at differnt stage).
#' x <- c("2023-01-01", "2022/12/31", "2002-07-02")
#' p <- "^([0-9]{2,4})[-/]([0-9]{1,2})[-/]([0-9]{1,2})$"
#' ## reference usage: input x appeared twice
#' (a0 <- do.call(rbind, regmatches(x, regexec(p, x))))
#' ## composed function 1: blank as input from upstream, `0` as initial input.
#' (f1 <- regexec(p,  ) %.% regmatches(`0`,  ) %.% do.call(rbind,  ))
#' (a1 <- f1(x))
#' ## composed function 2: dot refer to upstream, `0` as initial input.
#' (f2 <- regexec(p, .) %.% regmatches(`0`, .) %.% do.call(rbind, .))
#' (a2 <- f2(x))
#' ## composed function 3: use of named argument, `0` as initial input.
#' (f3 <- regexec(pa=p) %.% regmatches(x =`0`) %.% do.call(wh=rbind))
#' (a3 <- f3(x))
#'
#' ## ex2: date given days since 2000-010-1 (shorthand form of function calls)
#' (g1 <- as.Date(origin="2000-01-01") %.% f2()) # reused composed `f2`
#' (b1 <- g1(364))
#' (g2 <- as.Date(origin="2000-01-01") %.% f2)   # omit ()
#' (b2 <- g2(364))
#' 
#' ## ex3: wrap, pad, and upcase the strings (`sapply` uses composed function)
#' words <- c("Hellow World!", "Good Morning!")
#' s0 <- sapply(words, function(x)
#' {
#'     toupper(paste(format(strwrap(x, 8), w=12, just="c"), collapse="\n"))
#' })
#' cat(s0, sep="\n---- s0 ----\n")
#' s1 <- sapply(words, strwrap(8) %.% format(w=12, just="c") %.%
#'                     paste(collapse="\n") %.% toupper)
#' cat(s1, sep="\n---- s1 ----\n")
#'
#' ## check equivalance
#' all(a1==a0) && all(a2==a0) && all(a3==a0) && all(b1==b2) && all(s1==s0)
#' @export
fcp <- function(f=NULL, g)
{
    dot <- as.name(".")
    ## try find the function from a call
    fun <- function(x) tryCatch(get(x[[1]], mode="function"), error=function(e) "NULL")
    f <- substitute(f) # inner function (LHS of %.%)
    g <- substitute(g) # outer function (RHS of %.%)
    
    ## call stacks
    . <- sapply(sys.calls(), fun)             # all function calls
    . <- sapply(., identical, sys.function()) # calls of `fcp`
    .d. <- sum(.)                             # depth of the calls
    .p. <- parent.frame(max(which(.)))        # first frame that called fcp
    
    ## treat the call  of `g` - RHS of a `f o g` expression
    g <- as.list(g)
    if(any(. <- sapply(g, deparse) == "")) # fill empty args with `.`
        g[.] <- replicate(sum(.), dot)
    if(!any(sapply(g, `==`, dot))) # put a `.` to the front if none exist.
        g <- append(g, dot, 1)
    g <- as.call(g)
    
    ## treatment of `f`, the LHS of a `f` o `g`
    if(is.null(f)) # `f` is NULL, so `g` was the left most function.
    {
        . <- NULL
    }
    else if(identical(fun(f), sys.function()))
    {
        . <- eval(f, .p.) # `f` is also a fcp (shorter), recursively evaluate `f`
    }
    else # `f` is not a fcp-op, so `f` o `g` is already the left most fcp-op
    {
        . <- eval(call("fcp", g=f), .p.) # evaluate fcp(NUL, `f`)
    }
    
    ## stack up function calls
    G <- append(., g) # accumulate functions
    
    ## at the end of a deeper scp-op, pile up arguments and functions
    if(.d. > 1)
        return(G)

    ## at the end of the 1st scp-op, build a function from the pile.
    G <- sapply(seq_along(G), function(i) # add '. <- .d.' before each function call.
    {
        g <- deparse(G[[i]])
        g[1] <- sprintf("`%d` <- . <- %s", i, g[1])
        parse(text=g) 
    })
    G <- append(G, parse(text="`0` <- ."), 0) # name the pip-in as 0.

    ## build the function now
    ret <- function() {}                          # skeleton
    body(ret) <- as.call(c(as.name("{"), G, dot)) # body
    formals(ret) <- alist(.=)                     # argument
    environment(ret) <- parent.frame() ## as defined iin the calling env.
    ret
}

#' @rdname fcp
#' @export
`%.%` <- fcp

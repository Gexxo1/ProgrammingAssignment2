#----------------------------------------------------------------------
# Panoramica:
# Queste funzioni utilizzano lo scoping lessicale di R per memorizzare
# l'inversa di una matrice in cache, evitando di ricalcolarla.
#----------------------------------------------------------------------


# makeCacheMatrix
#
# Crea un oggetto (una lista) che contiene la matrice
# e le funzioni per gestire la sua inversa memorizzata in cache.
#
makeCacheMatrix <- function(mat = matrix()) {
    
    # 'cachedInverse' memorizza l'inversa calcolata.
    # Inizializzata a NULL e resettata ogni volta che la matrice cambia.
    cachedInverse <- NULL
    
    # Funzione 'set': aggiorna la matrice e resetta la cache
    set <- function(newValue) {
        mat <<- newValue
        cachedInverse <<- NULL # Invalida la vecchia cache
    }
    
    # Funzione 'get': restituisce la matrice
    get <- function() mat
    
    # Funzione 'setInverse': memorizza l'inversa calcolata
    setInverse <- function(inverse) cachedInverse <<- inverse
    
    # Funzione 'getInverse': restituisce l'inversa dalla cache
    getInverse <- function() cachedInverse
    
    # Restituisce una lista di funzioni
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# cacheSolve
#
# Calcola l'inversa della matrice speciale creata da 'makeCacheMatrix'.
# Prima controlla se l'inversa è già presente nella cache.
#
cacheSolve <- function(x, ...) {
    # Prova a recuperare l'inversa dalla cache
    inv <- x$getInverse()
    
    # Se la cache esiste (non è NULL), la restituisce
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Altrimenti, recupera la matrice...
    data <- x$get()
    
    # ...calcola l'inversa usando solve()...
    inv <- solve(data, ...)
    
    # ...la memorizza nella cache...
    x$setInverse(inv)
    
    # ...e la restituisce.
    inv
}

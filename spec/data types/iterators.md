# Iterators

## Abstract
Iterators are a way to go through a data stream without having to load the entire stream at once.
You can access elements one by one. Convenient usecasses are for example:
- Loading data from a large file, where you only need to look at parts of the file at any one time
- Data that is being created in real time. For example Network Packet streams.


You can c and Semantic
An iterator itself is an opaque datatype. Which can be created the following eay:

```
custom_iterator = iterator(next_function, hasnext_fu,ction it)erator_data
```
Where `iterator` is a function supplied by the language.

Both `next_function` and `hasnext_function` take a custom `iterator_data` dictionary as input.
`hasnext_function` should return `true` if there is another object to be accessed via the iterator. `next_function` should return the next object available. If there is no object to access, it should throw.

You can access elements from an instantiated iterator the following way:

```
it = ...
element = none
if (hasnext(it)) {
    element = next(it)
}
```

Where `next` and `hasnext` are functions provided by the language.

These are meant to give an understanding of how iterators are expected to work. 
They should not be taken as a guideline for how iterators need to be implemented

```
filter = (it, filterFn) => {
    hasnext_ = (d) => {
    
        if (d.hasBuffer) {
            return true
        }
        
        while (hasnext(d.it)) {
            n = next(d.it)
            
            if (d.filterFn(n)) {
                d.hasBuffer = true
                d.buffer = n
                return true
            }
        }
        
        return false
    }
    
    next_ = (d) => {
        if (hasnext_(d)) {
            d.hasBuffer = false
            return d.buffer
        }
        error()
    }
    
    data = { it: it, buffer: none, hasBuffer: false }

    return iterator(next_, hasnext_, data)
}
// Here, filter expects an iterator it and a function filterFn

// data defines the data dictionary that is passed to hasnext_ and next_ It contains an iterator, a buffer and a bool hasBuffer

// hasnext_ expects a dictionary conforming to the format defined by data It checks whether or not the iterator contains an element that satisfies the function used for filtering, then writes it into the buffer contained in datta

// next_ expects the same dictionary as hasnext and returns the buffered value, if one exists. If hasnext was unable to buffer a value, the function throws an error

// Lastly, a new iterator is returned that only returns values that satisfy the filterFn using next_ and hasnext_, as well as data to keep track of its state

listToIterator = (lst) => {
    hasnext_ = (d) => {
       return curInd < len(lst) 
    }
    next_ = {
    if (not hasnext_(d)) {
            error()
        }
    
        e = lst[curInd]
        curInd = curInd + 1
        return e
    }
    
    d = { arr: copy(lst), curInd: 0 }
    
    return iterator(next_, hasnext_, d)
}


// This function expects a list and uses a dict data that contains a copy of the passed list as well as tracking the current index

// hasnext_ returns true if the list in data contains another element, else false

// next_ returns the next element in the list contained in data if it exists, else it throws an error

// Then a new iterator is created and returned using hasnext_, next_ and data



TumblingWindow = (it, window_size, step_size) => {
    hasnext_ = (d) => hasnext(d.it)
    
    next_ = (d) => {
    
        if (not hasnext(d.it) and len(d.buf) == 0) {
            error()
        }
    
        while (hasnext_(d) and len(d.buf) < d.w) {
            d.buf = d.buf + [next(d.it)]
        }
        
        rv = d.buf | copy
        
        if (len(d.buf) > d.w - d.s) {
            d.buf = d.buf[d.s:]
        }
        
        return rv
    }
    
    d = { it: it, w: window_size, s: step_size, buf: [] }
    
    return iterator(hasnext_, next_, d)
}
// This function expects an iterator, as well as a window size and a step size for how many values the window should be moved inbetween each call of. It also uses a dict data to keep track of the iterator, window size, step size and a buffer containing the values in a window

// hasnext_ simply checks if the iterator in data contains another element

// next_ reads elements from the iterator and writes them to the buffer until the buffer is 'full' (contains window_size many elements) or there are no more elements in the iterator. It then writes them to an array. Then it removes step_size many from the front of the array in order to guarantee that the next call of next_ returns the next window. Then it returns the array containing the window. If next_ is called and both the buffer is empty and no more elements are in the iterator we are windowing over, an error is thrown.

// Then a new iterator using hasnext_, next_ and data is returned

```
## Built-ins

The following functions are provided by the language:

`filter(it: iterator[object], fn: func[object -> bool]) -> iterator[object]` - Returns an iterator, which will returns all elements of `it` that satisfy `fn`.

`map(it: iterator[object], fn: func[object -> object]) -> iterator[object]` - Returns an iterator, that returns every element in `it` and applies `fn` to it.

`reduce(it: iterator[object], fn: func[object, object -> object], default=undefined) -> object` - Applies `fn` first to the first 2 elements, then to the result of this operation and the thrid, and so on. If `default` is set, use it as the zero'th item in `it`. Otherwise `it` must have at least one arguments. 

`first(it: iterator, fn: func[object -> bool], default=undefined) -> object` - Returns the first object in `it` that satisfies `fn`. If no such object exists, throw an error if `default` is not set. Otherwise return `default`.

`groupBy(it: iterator, fn: func[object -> object]) -> iterator[iterator[object]]` - Returns an an iterator `i1`. `i1` number Of dictionaries corresponging to the groups, `fn` returns `{key: ..., values: i2}`. `i2` is an iterator that returns all values corresponding to that key.
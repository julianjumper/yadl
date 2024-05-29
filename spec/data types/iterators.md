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
  t_ = (        if (not hasnext_(d)) {
    next    {
        )
        }
    
        e = lst[curInd]
        curInd = curInd + 1
        return e
    }
    
    d = { arr: copy(lst), curInd: 0 }
    
    return iterator(next_, hasnext_, d)
}

t
// This function expects a list a
umblingWindow = (it, window_size,nd needs an array data that conforms to containiyng an array as well as tracking the current ind

xe step_size) => {
xexT    hasnext_ = (d) => hasnext(d.it)
    
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
```

## Built-ins



The following functions are provided by the language:

`filter(it: iterator, fn: function)` - Returns an iterator, which will retur
a all elements of `it` that satisfy `fn`, where `fn` takes an element and returns a boolean.


it = load "weatherdata.txt" as characters | lines | csv | json



1,2,3
a,b,c
x,y,z

{
a: 1,
b: {
 c: 3
}
}

- > it -> {key: a, value: 1}, { key:b, value: it }

[1,2,]

dict_rec(it) <- json as dict.
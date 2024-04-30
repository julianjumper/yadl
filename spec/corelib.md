```
map = (arr, fn) => {
    i = 0
    l = len(arr)
    while (i < l) {
        arr[i] = fn(arr[i])
    }
    return arr
}

filter = (arr, fn) => {
    i = 0
    j = 0
    l = len(arr)
    brr = []
    while (i < l) {
        if (fn(arr[i])) {
            brr[j] = arr[i]
            j = j + 1
        }
        i = i + 1
    } 
    return brr
}

```
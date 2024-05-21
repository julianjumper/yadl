import java.util.ArrayList;
import java.util.List;

enum Operation {
    EQUALS, GREATER, LESS
}

public interface Filter<T> {
    boolean apply(T item);
}



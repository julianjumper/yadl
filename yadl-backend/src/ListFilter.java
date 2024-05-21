import java.util.ArrayList;
import java.util.List;

enum Operation {
    EQUALS, GREATER, LESS
}
public class ListFilter {
    public static <T> List<T> filter(List<T> list, Filter<T> filter) {
        List<T> result = new ArrayList<>();
        for (T item : list) {
            if (filter.apply(item)) {
                result.add(item);
            }
        }
        return result;
    }

    public static <T extends Comparable<T>> List<T> filter(List<T> list, Operation operation, T value) {
        return filter(list, item -> {
            return switch (operation) {
                case EQUALS -> item.compareTo(value) == 0;
                case GREATER -> item.compareTo(value) > 0;
                case LESS -> item.compareTo(value) < 0;
                default -> throw new IllegalArgumentException("Unsupported operation: " + operation);
            };
        });
    }

    public static void main(String[] args) {
        List<Integer> numbers = List.of(1, 2, 3, 4, 5, 6);

        List<Integer> evens = filter(numbers, item -> item % 2 == 0);
        System.out.println("Evens: " + evens);

        List<Integer> odds = filter(numbers, item -> item % 2 != 0);
        System.out.println("Odds: " + odds);

        List<Integer> greaterThanThree = filter(numbers, Operation.GREATER, 3);
        System.out.println("Greater than 3: " + greaterThanThree);

        List<Integer> equalToThree = filter(numbers, Operation.EQUALS, 3);
        System.out.println("Equal to 3: " + equalToThree);

        List<Integer> customGreaterThanFour = filter(numbers, item -> item > 4);
        System.out.println("Greater than 4: " + customGreaterThanFour);
    }
}

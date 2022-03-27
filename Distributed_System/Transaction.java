//Instuctions: 1) Show transaction with the balance zero. 2) Compute monthly withdraw, deposit, and balance from banking transaction with 5000 and 5,000,000 records.
//Using sequential and parallel stream.

import java.io.File;
import java.nio.file.Files;

import java.util.stream.Stream;
import java.io.IOException;
import java.util.*;

import java.util.List;
import java.util.stream.Collectors;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.toList;

public class Transaction {

    public static void main(String[] args) throws IOException {
        long startTime = System.nanoTime();
        String filePath = "/Users/Asus/Desktop/5000 BT Records.csv";
        // String filePath = "/Users/Asus/Desktop/5000000 BT Records.csv";
        File file = new File(filePath);
        Stream<String> text = Files.lines(file.toPath());

        Map<String, List<String>> month = text.parallel().skip(1)
                .collect(groupingBy(line -> line.split(",|-", 2)[1].split(",", 5)[0]));

        List<String> date;
        date = month.entrySet().stream().map(pair -> pair.getKey()).collect(toList());
        // date.forEach(System.out::println);
        // System.out.println(date);

        // Deposit
        List<Double> deposit = month.entrySet().stream()

                .map(pair -> pair.getValue().stream().mapToDouble(GetData::getDeposit).sum()).collect(toList());
        // deposit.forEach(System.out::println);

        // Withdraw
        List<Double> withdraw = month.entrySet().stream()

                .map(pair -> pair.getValue().stream().mapToDouble(GetData::getWithdraw).sum()).collect(toList());
        // withdraw.forEach(System.out::println);

        // Balance
        List<Double> balance = month.entrySet().stream()
                .map(pair -> pair.getValue().stream().mapToDouble(GetData::getBalance).sum()).collect(toList());
        // balance.forEach(System.out::println);

        String[] array1 = date.toArray(new String[date.size()]);
        Double[] array2 = deposit.toArray(new Double[deposit.size()]);
        Double[] array3 = withdraw.toArray(new Double[withdraw.size()]);
        Double[] array4 = balance.toArray(new Double[balance.size()]);

        System.out.println("  Month  |    Withdraw   |     Deposit   |    Balance   ");
        System.out.println("---------------------------------------------------------");
        for (int i = 0; i < date.size(); i++) {
            System.out.println(array1[i] + " | " + array2[i] + " | " + array3[i] + " | " + array4[i]);
        }

        text.close();

        long endTime = System.nanoTime();
        long totalTime = endTime - startTime;
        double totalTimeInSecond = (double) totalTime / 1_000_000_000;

        System.out.println("Running time = " + totalTimeInSecond + " seconds");

    }
}

class GetData {

    public static double getDeposit(String record) {
        String[] tokens = record.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1);
        return Double.parseDouble(tokens[2].replaceAll(",", "").replaceAll("\"", ""));
    }

    public static double getWithdraw(String record) {
        String[] tokens = record.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1);
        return Double.parseDouble(tokens[3].replaceAll(",", "").replaceAll("\"", ""));
    }

    public static double getBalance(String record) {
        String[] tokens = record.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1);
        return Double.parseDouble(tokens[4].replaceAll(",", "").replaceAll("\"", ""));

    }
}

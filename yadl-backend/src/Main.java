import org.apache.flink.*;
import org.apache.flink.api.common.JobExecutionResult;
import org.apache.flink.api.common.functions.FilterFunction;
import org.apache.flink.api.common.functions.FlatMapFunction;
import org.apache.flink.api.java.DataSet;
import org.apache.flink.api.java.ExecutionEnvironment;
import org.apache.flink.api.java.tuple.Tuple2;
import org.apache.flink.shaded.jackson2.com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.flink.streaming.api.datastream.DataStream;
import org.apache.flink.streaming.api.environment.StreamExecutionEnvironment;
import org.apache.flink.util.Collector;

import java.io.File;

public class Main {

    public static void main(String[] args) throws Exception {
        customObject tmp = new ObjectMapper().readValue(new File("./res/test.json"), customObject.class);
        for(GAO i : tmp.root.values())
        {
            if(i.type == GAO.Type.Int)
            {
                System.out.println(i.integerData);
            }
            else
            {
                System.out.println(i.stringData);
            }
        }
        /*ExecutionEnvironment env = ExecutionEnvironment.createLocalEnvironment();

        DataSet<String> data = env.readTextFile("C:/Users/Phyton/Desktop/Test.txt");
        for(String i : data.collect())
        {
            System.out.println(i);
        }
        data.filter(new FilterFunction<String>() {
                    public boolean filter(String value) {
                        return value.startsWith("http://");
                    }
                });

        //JobExecutionResult res = env.execute();*/
    }

}

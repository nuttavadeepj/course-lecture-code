import java.io.IOException;
import java.util.List;
import java.util.ArrayList;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

class TransactionMapper extends Mapper<Object, Text, Text, Text> {

    private List<String> lines = new ArrayList();

    public void map(Object key, Text value, Context context) throws IOException, InterruptedException {
      String valueString = value.toString();
      if(valueString.equals("Date,Description,Deposits,Withdrawls,Balance")) return;
      int size = lines.size();

      if(size == 0) {
        lines.add(valueString);
      } else {
        String[] line = valueString.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1);
        String prevRecord = lines.get(size - 1);
        String[] prevLine = prevRecord.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)", -1);
        double prevBalance = Double.parseDouble(prevLine[4].replaceAll(",","").replaceAll("\"",""));
        double currentBalance = Double.parseDouble(line[4].replaceAll(",","").replaceAll("\"",""));
        double deposit = Double.parseDouble(line[2].replaceAll(",","").replaceAll("\"",""));
        double withdraw = Double.parseDouble(line[3].replaceAll(",","").replaceAll("\"",""));
        if(prevBalance + deposit - withdraw != currentBalance) {
          context.write(new Text(""),value);
        }
      }   
    }    
  }     

public class ErrorBalance { 
  public static void main(String[] args) throws Exception {
    Configuration conf = new Configuration();
    Job job = Job.getInstance(conf, "Balance");
    job.setNumReduceTasks(0);
    job.setJarByClass(ErrorBalance.class);
    job.setMapperClass(TransactionMapper.class);
    
    job.setOutputKeyClass(Text.class); 
    job.setOutputValueClass(Text.class);
    
    FileInputFormat.addInputPath(job, new Path(args[0]));
    FileOutputFormat.setOutputPath(job, new Path(args[1]));
    System.exit(job.waitForCompletion(true) ? 0 : 1);
  }
}
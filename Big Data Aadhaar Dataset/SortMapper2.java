
import java.io.IOException;

import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.LongWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Mapper;

public class SortMapper2 extends Mapper<LongWritable, Text, IntWritable, Text> {

	Text enrollmentagency = new Text();
	IntWritable total = new IntWritable();

	public void map(LongWritable key, Text value, Context context)
			throws IOException, InterruptedException {
		String[] splits = value.toString().split("\\|");

		enrollmentagency.set(splits[0].trim());
		total.set(Integer.parseInt(splits[1].trim()));

		context.write(total, enrollmentagency);
	}
}
package apiCallers;

import java.io.IOException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;

public class InitGameApiCaller {

    public String sendRequest(int numberOfPlayers) {
        String url = "http://localhost:8081/game/init/";
        String getUrl = url + numberOfPlayers;
        HttpGet httpGet = new HttpGet(getUrl);

        try (CloseableHttpClient httpClient = HttpClients.createDefault();
             CloseableHttpResponse response = httpClient.execute(httpGet)) {

            return(EntityUtils.toString(response.getEntity()));
        }
        catch (IOException e) {
            e.printStackTrace();
            return("failed");
        }
    }
}

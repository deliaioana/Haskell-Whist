package apiCallers;

import game.Game;
import org.apache.http.HttpEntity;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.util.EntityUtils;

import java.io.IOException;

public class GetNumberOfRoundsApiCaller {
    public String sendRequest(String JSONGame) throws ClientProtocolException, IOException {
        CloseableHttpClient client = HttpClients.createDefault();
        HttpPost httpPost = new HttpPost("http://localhost:8081/game/getNumberOfRounds");

        StringEntity entity = new StringEntity(JSONGame);
        httpPost.setEntity(entity);
        httpPost.setHeader("Accept", "application/json");
        httpPost.setHeader("Content-type", "application/json");

        CloseableHttpResponse response = client.execute(httpPost);
        client.close();

        HttpEntity responseEntity = response.getEntity();
        String stringResponse = "";
        if(responseEntity!=null) {
            stringResponse = EntityUtils.toString(responseEntity);
        }
        return stringResponse;
    }
}

Feature: Lotteries can be created and deleted

    Scenario: A lottery is created and a deletion token is created
        Given that I have started lottery
        When I create a lottery
        Then I get a 201 response code
        And I get a URL
        And I get a deletion token

    Scenario: A lottery is deleted by using its name and token
        Given that I have started lottery
        And a lottery has been created
        When I delete a lottery
        Then I get a 204 code

    Scenario: Deleting a lottery which hasn't been created returns a failure code
        Given that I have started lottery
        When I delete a non-existing lottery
        Then I get a 400 code

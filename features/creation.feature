Feature:
    Lotteries can be created and deleted

    Background:
        Given that I have started lottery

    Scenario: When you successfully create a lottery then you get a url and a
        deletion token back.
        When I create a lottery
        Then I get a 201 response code
        And I get a URL
        And I get a deletion token

    Scenario: A lottery is deleted by using its name and token
        Given that a lottery has been created
        When I delete that lottery
        Then I get a 204 code

    Scenario: Deleting a non-existing lottery returns a failure code
        When I delete a non-existing lottery
        Then I get a 400 code

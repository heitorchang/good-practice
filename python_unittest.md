# Unit testing

## To set up data or load resources once for all tests

```
import unittest
import requests

class MyObjectTest(unittest.TestCase):
    """
    Description
    """

    @classmethod
    def setUpClass(cls):
        super().setUpClass()
        data_str = requests.get("http://example.com/json").content
        cls.data = json.loads(data_str)


    def test_something(self):
        self.assertEqual(self.data['name'], "Object Name")


if __name__ == "__main__":
    unittest.main(warnings="ignore")
```
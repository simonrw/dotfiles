# Interface Design for Testability

Good interfaces make testing natural:

1. **Accept dependencies, don't create them**

   ```
   // Testable
   function process_order(order, payment_gateway) { ... }

   // Hard to test
   function process_order(order) {
       gateway = new StripeGateway()
   }
   ```

2. **Return results, don't produce side effects**

   ```
   // Testable
   function calculate_discount(cart) -> Discount { ... }

   // Hard to test
   function apply_discount(cart) -> void {
       cart.total -= discount
   }
   ```

3. **Small surface area**
   - Fewer methods = fewer tests needed
   - Fewer params = simpler test setup
